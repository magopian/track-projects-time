import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveSession.subscribe(data => {
  console.log("save session", data);
  localStorage.setItem("session", JSON.stringify(data));
});

app.ports.logoutSession.subscribe(() => {
  console.log("logout session");
  localStorage.removeItem("session");
});

registerServiceWorker();
