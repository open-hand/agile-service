import React, { useMemo } from 'react';
import {
  Modal, DataSet, Table, TextField, Icon,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import { useDebounceFn } from 'ahooks';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { ganttApiConfig } from '@/api';
import { systemColumnsMap } from '@/components/issue-table/baseColumns';
import ProjectTag from '@/components/tag/project-tag';
import SelectProject from '@/components/select/select-project';
import { LINK_URL_TO } from '@/constants/LINK_URL';

interface IGanttConflictModalProps {
    assigneeName:string,
    assigneeId: string,
    onOk?: Function
}
const GanttConflict: React.FC<{ modal?: IModalProps } & IGanttConflictModalProps> = observer(({
  modal, assigneeId, onOk,
}) => {
  const dataset = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    fields: [
      { name: 'projectId', label: '冲突项目', type: 'string' as FieldType },
      { name: 'summary', label: '概要', type: 'string' as FieldType },
      { name: 'issueNum', label: '编号', type: 'string' as FieldType },
      { name: 'estimatedStartTime', label: '预计开始时间', type: 'string' as FieldType },
      { name: 'estimatedEndTime', label: '预计结束时间', type: 'string' as FieldType },
    ],
    transport: {
      read: ({ params, data }) => ({ ...ganttApiConfig.loadTimeConflictDetail(assigneeId, { ...data }), params: { assigneeId, ...params } }),
    },
  }), [assigneeId]);

  const columns: ColumnProps[] = useMemo(() => {
    function getData(r: any, key: string) {
      return r.get(key);
    }
    function onClickSummary(r: any) {
      return () => LINK_URL_TO.issueLinkTo(r.get('issueId'), r.get('issueNum'), {}, r.get('projectId'));
    }
    const renderSummary: ColumnProps['renderer'] = ({ record }) => systemColumnsMap.get('summary')!.render!(record, getData, { onClick: onClickSummary(record) });
    return [
      {
        name: 'projectId', width: 210, sortable: true, renderer: ({ record }) => record?.get('project') && <ProjectTag data={record?.get('project')} showText />,
      },
      { name: 'summary', renderer: renderSummary },
      { name: 'issueNum', sortable: true, width: 100 },
      {
        name: 'estimatedStartTime', sortable: true, renderer: ({ value }) => value, width: 160,
      },
      {
        name: 'estimatedEndTime', sortable: true, renderer: ({ value }) => value, width: 160,
      },
    ] as ColumnProps[];
  }, []);
  const { run: query, flush } = useDebounceFn(() => {
    dataset.query();
  }, { wait: 270 });

  return (
    <div>
      <div style={{ marginBottom: 10 }}>
        <TextField
          prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
          placeholder="请输入搜索内容"
          onInput={(e: any) => {
            dataset.setQueryParameter('contents', e.target.value);
            query();
          }}
          onChange={(value) => {
            dataset.setQueryParameter('contents', value);
            query();
            flush();
          }}
        />
        <SelectProject
          style={{ marginLeft: 10, minWidth: 260 }}
          placeholder="冲突项目"
          label="冲突项目"
          flat
          multiple
          maxTagCount={3}
          queryAgile
          onChange={(value) => {
            dataset.setQueryParameter('teamProjectIds', value);
            query();
          }}
        />
      </div>
      <Table dataSet={dataset} columns={columns} queryBar={'none' as any} />
    </div>
  );
});
const openGanttConflictModal = (props: IGanttConflictModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: `${props.assigneeName}规划冲突提醒`,
    style: {
      width: MODAL_WIDTH.large,
    },
    okCancel: false,
    drawer: true,
    okText: '关闭',
    children: <GanttConflict {...props} />,

  });
};
export default openGanttConflictModal;
