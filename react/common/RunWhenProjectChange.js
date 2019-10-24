import {
  stores,
} from '@choerodon/boot';
import { autorun } from 'mobx';

const { AppState } = stores;
let projectId = AppState.currentMenuType ? AppState.currentMenuType.id : 0;
let preProjectId = AppState.currentMenuType ? AppState.currentMenuType.id : 0;
const subscribers = [];
function subscribe(fn) {
  if (!subscribers.includes(fn)) {   
    subscribers.push(fn); 
  }
}
function unsubscribe(fn) {
  subscribers.splice(subscribers.indexOf(fn), 1);
}
autorun(() => {
  projectId = AppState.currentMenuType ? AppState.currentMenuType.id : 0;
  if (projectId !== preProjectId) {
    subscribers.forEach((fn) => {     
      fn();
      // 执行一次之后，自动取消订阅
      // unsubscribe(fn);
    });
    preProjectId = projectId;
  }
});

export default subscribe;
