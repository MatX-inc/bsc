/* A parser-only oracle. Never evaluate or source the input Tcl scripts. */
#include <tcl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void print_hex(const char *text, size_t size)
{
    for (size_t i = 0; i < size; i++)
        printf("%02x", (unsigned char)text[i]);
}

static int inspect_file(Tcl_Interp *interp, const char *path)
{
    FILE *file = fopen(path, "rb");
    if (!file) {
        perror(path);
        return 2;
    }
    if (fseek(file, 0, SEEK_END)) {
        perror(path);
        fclose(file);
        return 2;
    }
    long size = ftell(file);
    if (size < 0 || size > INT_MAX) {
        fprintf(stderr, "Unsupported input size: %s\n", path);
        fclose(file);
        return 2;
    }
    rewind(file);
    char *text = malloc((size_t)size + 1);
    if (!text) {
        fclose(file);
        return 2;
    }
    if (fread(text, 1, (size_t)size, file) != (size_t)size) {
        fprintf(stderr, "Cannot read entire input: %s\n", path);
        fclose(file);
        free(text);
        return 2;
    }
    fclose(file);
    text[size] = 0;
    printf("F\t");
    print_hex(path, strlen(path));
    puts("");
    const char *position = text;
    while (position < text + size) {
        Tcl_Parse parse;
        int status = Tcl_ParseCommand(interp, position,
                                      (int)(text + size - position), 0, &parse);
        if (status != TCL_OK) {
            const char *message = Tcl_GetStringResult(interp);
            printf("E\t%d\t%ld\t", parse.errorType,
                   (long)(parse.commandStart - text));
            print_hex(message, strlen(message));
            puts("");
            Tcl_FreeParse(&parse);
            break;
        }
        if (parse.numWords) {
            printf("C\t%ld\n", (long)(parse.commandStart - text));
            int token_index = 0;
            for (int word = 0; word < parse.numWords; word++) {
                Tcl_Token *token = &parse.tokenPtr[token_index];
                printf("W\t%ld\t", (long)(token->start - text));
                print_hex(token->start, (size_t)token->size);
                puts("");
                token_index += 1 + token->numComponents;
            }
            if (token_index != parse.numTokens) {
                fprintf(stderr, "Native token accounting failed: %s\n", path);
                Tcl_FreeParse(&parse);
                free(text);
                return 2;
            }
        }
        const char *next = parse.commandStart + parse.commandSize;
        Tcl_FreeParse(&parse);
        if (next <= position || next > text + size) {
            fprintf(stderr, "Native parser did not advance correctly: %s\n", path);
            free(text);
            return 2;
        }
        position = next;
    }
    puts("Z");
    free(text);
    return 0;
}

int main(int argc, char **argv)
{
    int major, minor, patch, release_type;
    Tcl_GetVersion(&major, &minor, &patch, &release_type);
    fprintf(stderr, "native Tcl parser %d.%d.%d\n", major, minor, patch);
    if (major != 8 || minor != 6) {
        fprintf(stderr, "This lexer comparison requires Tcl 8.6.\n");
        return 2;
    }
    Tcl_Interp *interp = Tcl_CreateInterp();
    int status = 0;
    for (int argument = 1; argument < argc && status == 0; argument++)
        status = inspect_file(interp, argv[argument]);
    Tcl_DeleteInterp(interp);
    return ferror(stdout) ? 2 : status;
}
