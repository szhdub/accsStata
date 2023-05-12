import { Elm } from '../elm/Main.elm'



const app = Elm.Main.init({
    node: document.querySelector('main'),
    flags: {
        host: window.location.hostname
    }
})