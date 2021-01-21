import React, { useEffect, useState } from 'react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import classnames from 'classnames';
import styles from './index.less';

const ListIcon = ({ className = '', onClick }: { className: string, onClick: () => void }) => (
  <div role="none" className={className} onClick={onClick}>
    <svg viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="776" width="200" height="200">
      <path d="M213.333333 584.832h140.544v-145.664H213.333333v145.664zM213.333333 810.666667h140.544v-145.706667H213.333333V810.666667zM213.333333 359.04h140.544V213.333333H213.333333v145.706667z m175.701334 225.792H810.666667v-145.664H389.034667v145.664z m0 225.834667H810.666667v-145.706667H389.034667V810.666667z m0-597.333334v145.706667H810.666667V213.333333H389.034667z" p-id="777" />
    </svg>
  </div>
);
interface Props {

  data: 'tree' | 'list'
  onChange?: (mode: 'list' | 'tree') => void
  disabled?: boolean
}
const TreeListSwitch: React.FC<Props> = ({ data: propsData, onChange, disabled }) => {
  const [data, setData] = useState(() => propsData);
  useEffect(() => {
    if (typeof (propsData) === 'string') {
      setData(propsData);
    }
  }, [propsData]);
  const handleChange = (mode: 'tree' | 'list') => {
    setData(() => {
      onChange && onChange(mode);
      return mode;
    });
  };
  return (
    <div className={styles.switch}>
      <Tooltip title="列表视图">
        <div className={classnames(styles.icon, styles.icon_list, data === 'list' ? styles.selected : undefined, { [styles.disabled]: disabled })}>
          <ListIcon onClick={() => handleChange('list')} className={styles.icon_list_wrap} />
          {/* <Icon type="view_list" onClick={() => handleChange('list')} className={classnames(styles.icon, styles.icon_list, data === 'list' ? styles.selected : undefined, { [styles.disabled]: disabled })} /> */}
        </div>
      </Tooltip>

      <Tooltip title="树形视图">
        <Icon type="frame" onClick={() => handleChange('tree')} className={classnames(styles.icon, styles.icon_tree, data === 'tree' ? styles.selected : undefined, { [styles.disabled]: disabled })} />
      </Tooltip>
    </div>
  );
};
export default TreeListSwitch;
