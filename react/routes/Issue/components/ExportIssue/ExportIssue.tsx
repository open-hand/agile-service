import React, { Component, ReactElement, useState } from 'react';
import { stores, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { omit, find, unionBy } from 'lodash';
import {
  Radio, Divider, Icon, Row, Col, Dropdown, Menu,
} from 'choerodon-ui';
import FileSaver from 'file-saver';
import IssueStore, { getSystemFields } from '@/stores/project/issue/IssueStore';

import { issueApi } from '@/api';
import {
  DataSet, Table, Form, Select, Button, CheckBox,
} from 'choerodon-ui/pro';
import SelectSprint from '@/components/select/select-sprint';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import IssueFilterForm from '@/components/issue-filter-form';
import ChooseField from '@/components/chose-field';
import TableColumnCheckBoxes from '@/components/table-column-check-boxes';
import { useExportIssueStore } from './stores';
import WsProgress from './components/ws-progress';

interface FormPartProps {
  title: string | ReactElement,
  children: ReactElement | ReactElement[] | null,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}
const FormPart: React.FC<FormPartProps> = (props) => {
  const {
    title, children, btnOnClick,
  } = props;
  const { prefixCls } = useExportIssueStore();
  const [btnStatus, setBtnStatus] = useState<'ALL' | 'NONE'>();
  function handleClick() {
    let result = true;
    const nextBtnStatus = btnStatus !== 'NONE' ? 'NONE' : 'ALL';
    if (typeof (btnOnClick) === 'function') {
      result = btnOnClick(nextBtnStatus);
    }
    result && setBtnStatus(nextBtnStatus);
  }
  return (
    <div className={`${prefixCls}-form`}>
      <div className={`${prefixCls}-form-title`}>
        <span>{title}</span>
        {!!btnOnClick && (
          <Button
            className={`${prefixCls}-form-btn`}
            color={'blue' as ButtonColor}
            onClick={handleClick}
          >
            {btnStatus !== 'NONE' ? '全选' : '全不选'}
          </Button>
        )}
      </div>
      {children}
    </div>
  );
};
const fieldTransform = {
  issueNum: 'issueNum',
  issueId: 'summary',
  //  "description":
  issueTypeId: 'typeName',
  //  "projectName":
  assigneeId: 'assigneeName',
  // "assigneeRealName":
  reporterId: 'reporterName',
  //  "reporterRealName":
  //   "resolution":
  statusId: 'statusName',
  issueSprintVOS: 'sprintName',
  // "creationDate":
  lastUpdateDate: 'lastUpdateDate',
  priorityId: 'priorityName',
  //  "subTask":
  //  "remainingTime":
  version: 'versionName',
  epic: 'epicName',
  label: 'labelName',
  storyPoints: 'storyPoints',
  component: 'componentName',
};

const ExportIssue: React.FC<{}> = () => {
  const {
    prefixCls, checkOptions, tableDataSet, choseFieldStore,
    tableColumnCheckBoxesDataSet, issueFilterFormDataSet,
  } = useExportIssueStore();

  /**
 * 输出 excel
 */
  const exportExcel = () => {
    // console.log('dataSet', exportIssueDataSet.toData());
    // console.log('customFieldFilters', choseFieldStore.getCustomFieldFilters(exportIssueDataSet.current!));
    // console.log('exportFieldCodes', choseFieldStore.getExportFieldCodes(exportIssueDataSet.current!));
    const field = find(checkOptions, (f) => f.order) as { value: string, label: string, order?: string, };
    const search = {
      // ...choseFieldStore.getCustomFieldFilters(exportIssueDataSet.current!),
      // ...choseFieldStore.getExportFieldCodes(exportIssueDataSet.current!),
    };
    // @ts-ignore
    issueApi.export(search, field ? `${field.name},${field.order}` : undefined)
      .then((blobData: any) => {
        // const blob = new Blob([blobData], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
        // const fileName = 'AppState.currentMenuType.name.xlsx';
        // FileSaver.saveAs(blob, fileName);
        Choerodon.prompt('导出成功');
        // IssueStore.setExportModalVisible(false);
      }).finally(() => {

      });
  };
  const handleChangeFieldStatus = (status: 'ALL' | 'NONE') => {
    if (status !== 'ALL') {
      tableColumnCheckBoxesDataSet.current?.set('exportFieldCodes', checkOptions.map((column) => column.value));
    } else {
      tableColumnCheckBoxesDataSet.current?.set('exportFieldCodes', []);
    }
    return true;
  };

  return (
    <div>
      <FormPart title="筛选问题">
        <IssueFilterForm dataSet={issueFilterFormDataSet} chosenFields={choseFieldStore.getAllChosenField} onDelete={(item) => choseFieldStore.delChosenFields(item.code)}>
          <ChooseField store={choseFieldStore} />
        </IssueFilterForm>
      </FormPart>
      <Divider className={`${prefixCls}-horizontal`} />
      <FormPart title="选择字段" btnOnClick={handleChangeFieldStatus}>
        <TableColumnCheckBoxes options={checkOptions} dataSet={tableColumnCheckBoxesDataSet} name="exportFieldCodes" />
        <Button icon="unarchive" style={{ color: '#3f51b5' }} onClick={exportExcel}>导出问题</Button>
      </FormPart>
      <WsProgress messageKey="agile" />
    </div>
  );
};

export default observer(ExportIssue);
