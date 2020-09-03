import React, { useContext } from 'react';
import context from '../context';
import styles from './index.less';

interface Props {
  activeKey: React.Key | null
  onClose: () => void
}
const PanelList: React.FC<Props> = ({ activeKey, onClose }) => {
  const { tabs } = useContext(context);
  return (
    <div className={styles.panel_list}>
      {tabs.map((tab) => React.cloneElement(tab.node, {
        key: tab.key,
        tabKey: tab.key,
        active: tab.key === activeKey,
        onClose,
      }))}
    </div>
  );
};
export default PanelList;
