import React, { useMemo } from 'react';
import {
  Modal, DataSet, Form, Select, Button, Icon,
} from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './index.less';

interface IGanttDependencyModalProps {
  onOk?: Function
}
const options = ['FF', 'FS', 'SF', 'SS'];
const GanttDependency: React.FC<{ modal?: IModalProps } & IGanttDependencyModalProps> = observer(({
  modal, onOk,
}) => {
  const dataset = useMemo(() => new DataSet({
    autoCreate: true,
    selection: false,
    fields: [
      {
        name: 'relationship',
        label: '依赖关系',
        required: true,
        type: 'string' as FieldType,
        options: new DataSet({
          paging: false,
          data: options.map((item) => ({ meaning: item, value: item })),
        }),
      },
      {
        name: 'issue', label: '工作项', required: true, type: 'string' as FieldType,
      },

    ],
    transport: {
    },
  }), []);

  return (
    <div>
      <Form dataSet={dataset}>
        {dataset.data.map((record) => (
          <div className={styles.item}>
            <Select name="relationship" record={record} className={styles.select} />
            <Select name="issue" multiple record={record} className={styles.select} />
            <Icon type="delete_sweep-o" onClick={() => dataset.delete(record, false)} className={styles.del_btn} />
          </div>
        ))}
      </Form>
      <Button icon="add" onClick={() => dataset.create()}>添加前置依赖</Button>
    </div>
  );
});
const openGanttDependencyModal = (props: IGanttDependencyModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: '添加前置依赖',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <GanttDependency {...props} />,

  });
};
export default openGanttDependencyModal;
