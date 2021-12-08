import React, { useContext } from 'react';
import classNames from 'classnames';
import context from '../context';
import styles from './index.less';
import { Tab } from '../PanelList/Panel';

interface Props {
  onTabClick: (key: React.Key) => void
  activeKey: React.Key | null
}
const NavList: React.FC<Props> = ({ onTabClick, activeKey }) => {
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
          key={tab.key}
          className={classNames(styles.nav_item, {
            [styles.nav_item_active]: activeKey === tab.key,
          })}
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
