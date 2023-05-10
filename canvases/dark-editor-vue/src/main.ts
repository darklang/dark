import { createApp } from 'vue'
import App from './App.vue'
import './assets/tailwind.css'

/* import the fontawesome core */
import { library } from '@fortawesome/fontawesome-svg-core'

/* import font awesome icon component */
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

/* import specific icons */
import { fas } from '@fortawesome/free-solid-svg-icons'

/* add icons to the library */
library.add(fas)

const app = createApp(App)

const handleDarkResult = (message: string) => {
  console.log('handleDarkResult', message)
}

app.config.globalProperties.window = {
  handleDarkResult,
}
app.component('fa', FontAwesomeIcon).mount('#app')
