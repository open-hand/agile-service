import React, { useContext } from 'react';
import context from '../context';
import styles from './index.less';
import { Tab } from '../PanelList/Panel';

interface Props {
  onTabClick: (key: React.Key) => void
}
const NavList: React.FC<Props> = ({ onTabClick }) => {
  const { tabs } = useContext(context);
  const renderNav = (tab: Tab) => {
    if (tab.nav) {
      if (typeof tab.nav === 'function') {
        return tab.nav(tab.title);
      }
      return tab.nav;
    }
    return tab.title;
  };
  return (
    <div className={styles.nav_list}>
      {tabs.map((tab) => (
        <div
          role="none"
          className={styles.nav_item}
          onClick={() => {
            onTabClick(tab.key);
          }}
        >
          {renderNav(tab)}
        </div>
      ))}
    </div>
  );
};
export default NavList;
