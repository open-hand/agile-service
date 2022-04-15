import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import STATUS_TYPE from '@/constants/STATUS_TYPE';
import { IStatus } from '@/common/types';
import BaseTag from '../base-tag';
import styles from './index.less';

interface Props {
  mode?: 'inline' | 'tag'
  code: IStatus['valueCode']
  name?: string
  tooltip?: boolean
}
const renderInlineMode = ({ color, name }: { color: string, name: string }) => (
  <div className={styles.status_type_inline_mode}>
    <div
      className={styles.status_type_block}
      style={{
        background: color || 'rgb(255, 177, 0)',
      }}
    />
    {name}
  </div>
);
const StatusTypeTag: React.FC<Props> = ({
  mode = 'inline', code, name: propsName, tooltip,
}) => {
  const type = STATUS_TYPE[code] ?? {};
  const { color } = type;
  const name = propsName ?? type.name;
  switch (mode) {
    case 'inline': return renderInlineMode({
      color,
      name,
    });
    case 'tag': {
      const tag = <div className={styles.status_tag}><BaseTag color={color} text={name} /></div>;
      return tooltip ? <Tooltip title={name}>{tag}</Tooltip> : tag;
    }
    default: return null;
  }
};
export default StatusTypeTag;
