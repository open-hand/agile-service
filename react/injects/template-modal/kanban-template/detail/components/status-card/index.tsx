import React from 'react';
import classNames from 'classnames';
import { Radio } from 'choerodon-ui/pro';
import { IKanbanTemplateStatus } from '@/api';
import { observer } from 'mobx-react-lite';
import StatusTypeTag from '@/components/tag/status-type-tag';
import styles from './index.less';

const grid = 12;
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
  columnId: string
  data: IKanbanTemplateStatus
}
const StatusCard: React.FC<ColumnProps> = ({
  className,
  index,
  columnId,
  data,
  ...otherProps
}) => (
  <div
    className={classNames(styles.status_card, className)}
    style={{
      margin: `${grid}px 0 `,
    }}
  >
    <StatusTypeTag
      mode="tag"
      code={data.categoryCode}
      name={data.name}
    />
    <br />
    <div role="none" className={styles.radio}>
      <Radio checked={data.templateCompleted}>设置已完成</Radio>
    </div>
  </div>
);

export default observer(StatusCard);
