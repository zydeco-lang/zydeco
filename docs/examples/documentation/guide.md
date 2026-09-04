# Reading a counter

The counter exposes one public field, [value](zydeco:member:./value).
Importing the implementation checks it against its companion signature, so its contract is the same
whether you read the generated reference or inspect a projected field in the editor.

```zydeco check
let counter = @(import("counter.zy")) in counter/value
```

In VS Code, place the cursor on `value` and choose **Zydeco: Show Documentation**.
Use **Pin** to keep that source occurrence selected while editing elsewhere.
The **Check** action verifies an authored example; **Open scratch** creates an editable copy with
its import paths preserved.
