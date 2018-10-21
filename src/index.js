import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const sessionData = localStorage.getItem("session");

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: sessionData
});

app.ports.saveSession.subscribe(sessionData => {
  localStorage.setItem("session", JSON.stringify(sessionData));
});

app.ports.logoutSession.subscribe(() => {
  localStorage.removeItem("session");
});

registerServiceWorker();
