async function fetchReport() {
    try {
        const response = await fetch('http://127.0.0.1:8080/report', {
            headers: { 'Authorization': 'Bearer SECRET_TOKEN' }
        });
        
        if (!response.ok) {
            throw new Error(`Error ${response.status}: ${response.statusText}`);
        }

        const data = await response.text();
        document.getElementById("report").innerText = data;
    } catch (error) {
        document.getElementById("report").innerText = `❌ Failed to load report: ${error.message}`;
    }
}
