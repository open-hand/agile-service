import { createContext } from 'react';
import { Tab } from './PanelList/Panel';

interface IExpandContext {
  tabs: Tab[]
}

const context = createContext<IExpandContext>({
  tabs: [],
});
export default context;
