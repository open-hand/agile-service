import React, { useMemo, useCallback } from 'react';
import {
  Modal, Form, Select, Button, Icon,
} from 'choerodon-ui/pro';
import { observer, useComputed } from 'mobx-react-lite';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { usePersistFn } from 'ahooks';
import { noop } from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './index.less';
import useGanntDependencyModal, { IGanttDependencyModalProps, StoreProvider } from './stores';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { ganttApi } from '@/api';
import type { IGanttUpdateIssueDependencyItem } from '@/api';
import { GanttIssue } from '../../types';
import InlineIssueTag from '@/components/tag/inline-issue-tag';

const SelectIssue: React.FC<Partial<SelectProps> & { issueId: string, selectIds?: string[] }> = ({ issueId, selectIds, ...otherProps }) => {
  const config = useMemo((): SelectConfig<any> => ({
    textField: 'summary',
    valueField: 'issueId',
    request: ({ page, filter }) => ganttApi.loadDependableIssues({ currentIssueId: issueId, page }, { contents: [filter].filter(Boolean) as string[], otherArgs: { issueIds: selectIds } }),
    optionRenderer: InlineIssueTag.optionRenderer,
    renderer: InlineIssueTag.renderer,
    paging: true,
  }), [issueId, selectIds]);
  const props = useSelect(config);
  return <Select {...props} {...otherProps} />;
};
SelectIssue.defaultProps = {
  selectIds: [],
};

const GanttDependency: React.FC = observer(() => {
  const {
    dataset, modal, onOk: propsOnOk, issueId, data: editData,
  } = useGanntDependencyModal();
  const onOk = usePersistFn(propsOnOk || noop) as Exclude<typeof propsOnOk, undefined>;
  const selectTypes = useComputed(() => dataset.data.map((r) => r.get('predecessorType')), [dataset.data]);
  const predecessorTypeLength = dataset.getField('predecessorType')?.getOptions()?.length || 0;
  const handleSubmit = useCallback(
    async () => {
      if (!await dataset.validate()) {
        return false;
      }
      const data = dataset.toData().map((item: any) => item.predecessorId.map((i: any) => ({
        issueId,
        predecessorId: i,
        predecessorType: item.predecessorType,
      }))).flat() as IGanttUpdateIssueDependencyItem[];
      await ganttApi.updateIssueDependency(issueId, data);
      await onOk(data);
      return true;
    }, [dataset, issueId, onOk],
  );
  modal?.handleOk(handleSubmit);
  return (
    <div>
      <Form dataSet={dataset}>
        {dataset.data.map((record) => (
          <div className={styles.item}>
            <Select
              name="predecessorType"
              record={record}
              className={styles.select}
              optionsFilter={(optionRecord) => {
                const valueCode = optionRecord.get('valueCode');
                return !selectTypes.includes(valueCode) || valueCode === record.get('predecessorType');
              }}
            />
            <SelectIssue name="predecessorId" multiple record={record} className={styles.select} issueId={issueId} selectIds={editData[record.get('predecessorType')]} />
            <Icon type="delete_sweep-o" onClick={() => dataset.delete(record, false)} className={styles.del_btn} />
          </div>
        ))}
      </Form>
      <Button icon="add" disabled={dataset.length >= predecessorTypeLength} onClick={() => dataset.create()}>添加前置依赖</Button>
    </div>
  );
});

const openGanttDependencyModal = (props: IGanttDependencyModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: `${props.data ? '编辑' : '添加'}前置依赖`,
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: (
      <StoreProvider {...props}>
        <GanttDependency />
      </StoreProvider>),

  });
};
export default openGanttDependencyModal;
