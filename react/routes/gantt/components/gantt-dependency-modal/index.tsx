import React, {
  useMemo, useCallback, useRef, useState, useImperativeHandle,
} from 'react';
import {
  Modal, Form, Select, Button, Icon,
} from 'choerodon-ui/pro';
import { observer, useComputed } from 'mobx-react-lite';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { useCreation, usePersistFn } from 'ahooks';
import { difference, find, noop } from 'lodash';
import classnames from 'classnames';
import { Loading } from '@choerodon/components';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './index.less';
import useGanntDependencyModal, { IGanttDependencyModalProps, StoreProvider } from './stores';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { ganttApi } from '@/api';
import type { IGanttUpdateIssueDependencyItem } from '@/api';
import InlineIssueTag from '@/components/tag/inline-issue-tag';
import { wrapRequestCallback } from '@/components/select/utils';

const SelectIssue: React.FC<Partial<SelectProps> & { issueId: string, setLoading: React.Dispatch<React.SetStateAction<boolean>>, selectIds?: string[], isStopRequest: boolean, excludeIssueIds?: string[] }> = ({
  issueId, selectIds: propsSelectIds, record, name, excludeIssueIds: propsExcludeIssueIds, setLoading, isStopRequest, ...otherProps
}) => {
  const { multiple } = otherProps;
  const optionDataRef = useRef<any[]>([]);
  const excludeIssueIdsRef = useRef<string[]>();
  const selectIdsRef = useRef<string[]>();

  const args = useCreation(() => {
    if (isStopRequest) {
      return { excludeIssueIds: excludeIssueIdsRef.current, selectIds: selectIdsRef.current };
    }
    const currentSelectedIds = record?.get(name);
    const firstOptionData = optionDataRef.current.slice(0, 50);
    let selectIds = propsSelectIds;
    const differenceSelectIds = difference(currentSelectedIds, propsSelectIds || []);
    const isNeedChangeSelectIds = differenceSelectIds.some((id) => !find(firstOptionData, (option) => option.issueId === id));
    if (isNeedChangeSelectIds) {
      setLoading(true);
      selectIds = currentSelectedIds;
    }
    selectIdsRef.current = selectIds;
    excludeIssueIdsRef.current = difference(propsExcludeIssueIds, currentSelectedIds);
    return ({ excludeIssueIds: excludeIssueIdsRef.current, selectIds: selectIdsRef.current });
  }, [propsExcludeIssueIds, isStopRequest, record, name]);

  const config = useMemo((): SelectConfig<any> => ({
    textField: 'summary',
    valueField: 'issueId',
    requestArgs: args,
    request: wrapRequestCallback(({ page, filter, requestArgs }) => ganttApi.loadDependableIssues({ currentIssueId: issueId, page },
      { contents: [filter].filter(Boolean) as string[], otherArgs: { issueIds: requestArgs?.selectIds, excludeIssueIds: requestArgs?.excludeIssueIds } }), () => {
      setLoading(false);
    }),
    afterLoad: (data) => {
      optionDataRef.current = data;
    },
    optionRenderer: InlineIssueTag.createIssueTag({ multiple }),
    renderer: InlineIssueTag.createIssueTag({ multiple, rendererMode: true }),
    paging: true,
  }), [args, issueId, multiple, setLoading]);
  const props = useSelect(config);
  return <Select name={name} record={record} {...props} {...otherProps} />;
};
SelectIssue.defaultProps = {
  selectIds: [],
  excludeIssueIds: [],
};

const GanttDependency: React.FC = observer(() => {
  const {
    dataset, modal, onOk: propsOnOk, issueId, data: editData, title, forwardRef, disableAutoCreate, horizontal = false,
  } = useGanntDependencyModal();
  const [loading, setLoading] = useState(!disableAutoCreate);
  const [{ focusing, latestFocusRecord }, setFocusing] = useState({ focusing: false, latestFocusRecord: undefined as number | undefined });
  const onOk = usePersistFn(propsOnOk || noop) as Exclude<typeof propsOnOk, undefined>;
  const selectTypes = useComputed(() => dataset.data.map((r) => r.get('predecessorType')), [dataset.data]);
  const predecessorTypeLength = dataset.getField('predecessorType')?.getOptions()?.length || 0;
  const getData = useCallback(() => {
    const dependencyData = dataset.toData().map((item: any) => item.predecessorId.map((i: any) => ({
      issueId: issueId === '0' ? undefined : issueId,
      predecessorId: i,
      predecessorType: item.predecessorType,
    }))).flat() as IGanttUpdateIssueDependencyItem[];
    return dependencyData;
  }, [dataset, issueId]);
  const handleSubmit = useCallback(
    async () => {
      if (!await dataset.validate()) {
        return false;
      }
      const data = getData();
      await ganttApi.updateIssueDependency(issueId, data);
      await onOk(data);
      return true;
    }, [dataset, issueId, onOk],
  );
  const defaultSelectedIds = useMemo(() => [...Object.values(editData)].flat() as string[], [editData]);
  const excludeIssueIds = useComputed(() => (focusing ? [] : difference(dataset.data.map((r) => [...(r.get('predecessorId') || [])]).flat() as string[], defaultSelectedIds)), [dataset.data.length, focusing]);
  modal?.handleOk(handleSubmit);

  useImperativeHandle(forwardRef, () => ({
    getDependencyData: getData,
    dependencyDataSet: dataset,
  }));

  return (
    <Loading type="spin" display={loading} className={classnames(styles.loading, horizontal ? styles.loading_horizontal : '')}>
      <Form dataSet={dataset} hidden={!dataset.length} columns={horizontal ? 2 : 1} className={horizontal ? styles.horizontal : ''}>
        {dataset.data.map((record) => ([
          <Select
            name="predecessorType"
            record={record}
            className={styles.select}
            optionsFilter={(optionRecord) => {
              const valueCode = optionRecord.get('valueCode');
              return !selectTypes.includes(valueCode) || valueCode === record.get('predecessorType');
            }}
          />,
          <div className={styles.item}>
            <SelectIssue
              key={`predecessorType-${record.id}`}
              name="predecessorId"
              multiple
              record={record}
              className={styles.select}
              issueId={issueId}
              setLoading={setLoading}
              onPopupHiddenChange={(hidden) => {
                if (hidden) {
                  setFocusing({ focusing: false, latestFocusRecord: record.id });
                }
              }}
              isStopRequest={latestFocusRecord === record.id || focusing}
              onFocus={() => setFocusing({ focusing: true, latestFocusRecord: undefined })}
              excludeIssueIds={excludeIssueIds}
              selectIds={editData[record.get('predecessorType')]}
            />
            <Icon
              type="delete_sweep-o"
              onClick={() => {
                setFocusing(() => ({ focusing: false, latestFocusRecord: undefined }));
                dataset.delete(record, false);
              }}
              className={styles.del_btn}
            />
          </div>,
        ]))}
      </Form>
      <Button icon="add" disabled={dataset.length >= predecessorTypeLength} onClick={() => dataset.create()}>{`添加${title ?? '前置依赖'}`}</Button>
    </Loading>

  );
});

export const GanttDependencyIndex = (props: IGanttDependencyModalProps) => {
  const ref = useRef();
  const forwardRef = props.forwardRef ?? ref;
  return (
    <StoreProvider {...props} forwardRef={forwardRef}>
      <GanttDependency />
    </StoreProvider>
  );
};

const openGanttDependencyModal = async (props: IGanttDependencyModalProps) => {
  const editData = await ganttApi.loadDependencyByCurrentIssue(props.issueId);
  Modal.open({
    key: Modal.key(),
    title: `${props.data?.length ? '编辑' : '添加'}${props.title ?? '前置依赖'}`,
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <GanttDependencyIndex {...props} data={editData} />,

  });
};
export default openGanttDependencyModal;
