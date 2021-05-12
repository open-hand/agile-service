import React, {
  useCallback,
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  Button, Table,
  DataSet, DatePicker, Form, Modal, Radio, Select, TextField, Tooltip, TextArea,
} from 'choerodon-ui/pro/lib';
import { debounce, isEmpty, pick } from 'lodash';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import RadioGroup from 'choerodon-ui/lib/radio/group';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
// import './index.less';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import renderTags from '@/components/column-renderer/tags';
import {
  IAppVersionData, publishVersionApi, publishVersionApiConfig, versionApi,
} from '@/api';
import { Checkbox } from 'choerodon-ui';
import SelectTeam from '@/components/select/select-team';

const { Column } = Table;
interface PreviewResultModalProps {
    tableData: any[]
    handleOk?: () => any
    selectIssue:(issueId:string)=>void
}
const PreviewResult: React.FC<{ modal?: IModalProps } & PreviewResultModalProps> = ({
  modal, tableData, handleOk, selectIssue,
}) => {
  const [applicationId, setApplicationId] = useState<string>();
  const [versionType, setVersionType] = useState<string>('version');
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: false,
    selection: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      { name: 'summary', label: '概要' },
      { name: 'issueNum', label: '编号' },
      { name: 'status', label: '状态' },
      { name: 'priority', label: '优先级' },
      // { name: 'influenceVersion', label: '影响的版本' },

      { name: 'assigneeId', label: '经办人' },
      { name: 'createDate', label: '创建时间' },

    ],
    transport: {
      // submit: ({ data }) => (editData ? publishVersionApiConfig.update(editData.id, data[0]) : publishVersionApiConfig.create(data[0])),
    },
  }), []);

  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = pick(ds.current?.toData(), ['versionAlias', 'actualPublishDate', 'description']);

    // await ds.submit();
    const result = handleOk && await handleOk();
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
        modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  function renderSummary({ value }: RenderProps) {
    return (
      <Tooltip title={value} placement="topLeft">
        <span className="c7n-agile-table-cell-click">{value}</span>
      </Tooltip>
    );
  }
  return (
    <Table dataSet={ds}>
      <Column
        name="summary"
        className="c7n-agile-table-cell-click"
        onCell={({ record }) => ({
          onClick: () => {
            selectIssue(record.get('issueId'));
          },
        })}
        lock={'left' as any}
        width={210}
        renderer={renderSummary}
      />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" renderer={({ record }) => (record?.get('statusVO') ? renderStatus({ record }) : undefined)} />
      <Column name="priority" renderer={renderPriority} />
      <Column
        name="influenceVersion"
        renderer={({ record }) => {
          const influenceArr = record?.get('versionIssueRelVOS')?.filter((i: any) => i.relationType === 'influence') || [];
          return influenceArr.length > 0 ? (
            <Tooltip title={<div>{influenceArr.map((item: { name: string }) => <div>{item.name}</div>)}</div>}>
              {renderTags({ array: influenceArr, name: influenceArr[0].name })}
            </Tooltip>
          ) : undefined;
        }}
      />
      <Column
        name="assigneeId"
        className="c7n-agile-table-cell"
        renderer={({ value, record }) => (value ? (
          <UserTag data={{
            imageUrl: record?.get('assigneeImageUrl'),
            loginName: record?.get('assigneeLoginName'),
            realName: record?.get('assigneeRealName'),
            tooltip: record?.get('assigneeName'),
          }}
          />
        ) : '')}
      />
      <Column name="createDate" />

    </Table>
  );
};
function openPreviewResultModal(props: PreviewResultModalProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '预览结果',
    style: {
      width: MODAL_WIDTH.large,
    },
    drawer: true,
    cancelText: '关闭',
    footer: (okBtn:React.ReactNode, cancelBtn:React.ReactNode) => {
      const footer = '';
      return (
        <div>
          {/* {okBtn} */}
          <Button funcType={'raised' as any} color={'primary' as any}>替换问题tag信息</Button>

          <Button funcType={'raised' as any} color={'primary' as any}>增加问题tag信息</Button>
          {cancelBtn}
        </div>
      );
    },
    children: <PreviewResult {...props} />,

  });
}

export { openPreviewResultModal };
