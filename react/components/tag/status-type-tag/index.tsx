import React from 'react';
import STATUS_TYPE from '@/constants/STATUS_TYPE';
import { IStatus } from '@/common/types';
import styles from './index.less';

interface Props {
  mode?: 'inline' | 'tag'
  code: IStatus['valueCode']
}
const renderTagMode = ({ color, name }: { color: string, name: string }) => (
  <div
    className={styles.status_type_tag_mode}
    style={{
      background: color,
    }}
  >
    {name || ''}
  </div>
);
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
const StatusTypeTag: React.FC<Props> = ({ mode = 'inline', code }) => {
  const { color, name } = STATUS_TYPE[code];
  switch (mode) {
    case 'inline': return renderInlineMode({
      color,
      name,
    });
    case 'tag': return renderTagMode({
      color,
      name,
    });
    default: return null;
  }
};
export default StatusTypeTag;
