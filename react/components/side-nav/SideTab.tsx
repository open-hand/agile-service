import React, { useState, useEffect } from 'react';
import Context from './context';
import Panel, { Tab, PaneProps } from './PanelList/Panel';
import { toArray } from './utils';
import NavList from './NavList';
import PanelList from './PanelList';
import styles from './SideTab.less';

interface Props {
  onChange?: (activeKey: React.Key | null) => void
  activeKey?: React.Key
}
function parseTabList(children: React.ReactNode): Tab[] {
  // @ts-ignore
  return toArray(children).map((node: React.ReactElement<PaneProps>) => {
    if (React.isValidElement(node)) {
      const key = node.key !== undefined ? String(node.key) : undefined;
      return {
        key,
        ...node.props,
        node,
      };
    }
    return null;
  }).filter((tab) => tab);
}

interface SideTabComponent extends React.FC<Props> {
  Panel: typeof Panel
}
const SideTab: SideTabComponent = ({
  onChange, activeKey: propsKey, children,
}) => {
  const [activeKey, setActiveKey] = useState<React.Key | null>(propsKey || null);
  useEffect(() => {
    setActiveKey(propsKey || null);
  }, [propsKey]);
  const tabs = parseTabList(children);
  const handleChange = (key: React.Key | null) => {
    setActiveKey(key);
    if (onChange) {
      onChange(key);
    }
  };
  return (
    <Context.Provider value={{
      tabs,
    }}
    >
      <div className={styles.container}>
        <NavList
          onTabClick={(key) => {
            if (key === activeKey) {
              handleChange(null);
            } else {
              handleChange(key);
            }
          }}
          activeKey={activeKey}
        />
        <PanelList
          activeKey={activeKey}
          onClose={() => {
            handleChange(null);
          }}
        />
      </div>
    </Context.Provider>
  );
};
SideTab.Panel = Panel;
export default SideTab;
