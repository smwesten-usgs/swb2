
#include <iostream>     // For standard input/output
#include <fstream>      // For file reading and writing
#include <sstream>      // For reading entire file into a string
#include <string>       // For string manipulation

int main() {
    // Define the path to the file you want to modify
    const std::string filePath = "builddir/build.ninja";

    // Define the string you want to find and the string to replace it with
    const std::string target = "xilink.exe";
    const std::string replacement = "link.exe";

    // Open the input file for reading
    std::ifstream input(filePath);
    if (!input) {
        // If the file can't be opened, print an error and exit
        std::cerr << "Error: Cannot open input file.\n";
        return 1;
    }

    // Use a string stream to read the entire contents of the file
    std::ostringstream buffer;
    buffer << input.rdbuf();  // Read the whole file into the buffer
    std::string content = buffer.str();  // Convert buffer to a string
    input.close();  // Close the input file

    // Replace all occurrences of the target string with the replacement
    size_t pos = 0;
    while ((pos = content.find(target, pos)) != std::string::npos) {
        // Replace the target string at the found position
        content.replace(pos, target.length(), replacement);
        // Move past the replaced string to avoid infinite loop
        pos += replacement.length();
    }

    // Open the same file again, this time for writing (this will overwrite it)
    std::ofstream output(filePath);
    if (!output) {
        // If the file can't be opened for writing, print an error and exit
        std::cerr << "Error: Cannot write to output file.\n";
        return 1;
    }

    // Write the modified content back to the file
    output << content;
    output.close();  // Close the output file

    // Print a success message
    std::cout << "Replacement complete.\n";
    return 0;  // Exit successfully
}

