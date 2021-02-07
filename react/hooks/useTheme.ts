import { stores } from '@choerodon/master';
import { useObserver } from 'mobx-react-lite';

const { AppState } = stores;
export default function useTheme() {
  return useObserver(() => AppState.getCurrentTheme);
}
