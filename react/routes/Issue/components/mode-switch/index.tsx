import React, { useEffect, useState } from 'react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import classnames from 'classnames';
import styles from './index.less';

interface Props {

    data: 'tree' | 'list'
    onChange?: (mode: 'list' | 'tree') => void
    disabled?: boolean
}
const TableModeSwitch: React.FC<Props> = ({ data: propsData, onChange, disabled }) => {
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
        <Icon type="view_list" onClick={() => handleChange('list')} className={classnames(styles.icon, styles.icon_list, data === 'list' ? styles.selected : undefined, { [styles.disabled]: disabled })} />
      </Tooltip>
      <Tooltip title="树形视图">
        <Icon type="frame" onClick={() => handleChange('tree')} className={classnames(styles.icon, styles.icon_tree, data === 'tree' ? styles.selected : undefined, { [styles.disabled]: disabled })} />
      </Tooltip>
    </div>
  );
};
export default TableModeSwitch;
