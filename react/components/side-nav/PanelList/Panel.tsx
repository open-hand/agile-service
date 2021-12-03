import React, { useEffect } from 'react';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import styles from './index.less';

export interface PaneProps {
  title: React.ReactNode
  nav?: React.ReactElement | ((title: React.ReactNode) => React.ReactElement)
  tabKey: React.Key
  active: boolean
  onClose?: () => void
  noHeader?: boolean
}
export interface Tab extends PaneProps {
  key: React.Key;
  node: React.ReactElement;
}
const Panel: React.FC<PaneProps> = ({
  children, active, tabKey, title, onClose, noHeader,
}) => {
  const [visited, setVisited] = React.useState(false);
  useEffect(() => {
    if (active) {
      setVisited(true);
    }
  }, [active]);
  return (
    <div className={classNames(styles.panel_item, {
      [styles.panel_hidden]: !active,
    })}
    >
      {!noHeader && (
        <div className={styles.panel_header}>
          <div className={styles.panel_title}><span>{title}</span></div>
          <Icon
            className={styles.panel_close}
            type="first_page"
            onClick={() => {
              if (onClose) {
                onClose();
              }
            }}
          />
        </div>
      )}
      <div className={styles.panel_content}>
        {(active || visited) && children}
      </div>

    </div>
  );
};

export default Panel;
