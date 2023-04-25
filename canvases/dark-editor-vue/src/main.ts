import { createApp } from 'vue'
import App from './App.vue'
import './assets/tailwind.css'

const app = createApp(App)

const handleDarkResult = (message: string) => {
  console.log("handleDarkResult", message);
};

app.config.globalProperties.window = {
  handleDarkResult
}
app.mount('#app')