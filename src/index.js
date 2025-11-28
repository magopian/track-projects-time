import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const sessionData = localStorage.getItem("session");

const app = Elm.Main.init({
  flags: sessionData
	node: document.getElementById("root"),
});

app.ports.saveSession.subscribe((sessionData) => {
	localStorage.setItem("session", JSON.stringify(sessionData));
});

app.ports.logoutSession.subscribe(() => {
	localStorage.removeItem("session");
});

registerServiceWorker();

// Custom element to copy some content (provided as the "data-content" attribute on our custom element) to the clipboard.
class CopyToClipboard extends HTMLElement {
	constructor() {
		super();
		this._timeout = null;
		this._onClick = this._onClick.bind(this);
	}

	connectedCallback() {
		this.addEventListener("click", this._onClick);
		this.textContent = "📋";
		this.style.cursor = "pointer";
		this.title = "Copy the filtered descriptions to the clipboard";
	}

	_onClick() {
		const content = this.getAttribute("data-content");
		navigator.clipboard.writeText(content).then(
			() => {
				clearTimeout(this._timeout);
				this._timeout = setTimeout(() => {
					this.textContent = "📋";
				}, 3000);
				this.textContent = "✅"; // Display a green checkmark for 3 seconds as a feedback to the user.
			},
			() => {
				console.log("failed to copy the content", content);
			},
		);
	}

	disconnectedCallback() {
		clearTimeout(this._timeout);
	}
}

// Define our custom element.
customElements.define("copy-to-clipboard", CopyToClipboard);
