import React, {
  ReactElement, useEffect, useState,
} from 'react';
import { stores, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { omit, find, unionBy } from 'lodash';
import { Divider } from 'choerodon-ui';
import classnames from 'classnames';
import { issueApi } from '@/api';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import IssueFilterForm from '@/components/issue-filter-form';
import ChooseField from '@/components/chose-field';
import TableColumnCheckBoxes from '@/components/table-column-check-boxes';
import WsProgress from '@/components/ws-progress';
import { getProjectName } from '@/utils/common';
import { useExportIssueStore } from './stores';
import { getCustomFieldFilters, getExportFieldCodes } from './utils';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
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
    <div className={classnames(`${prefixCls}-form`, props.className)}>
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
interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}
const ExportIssue: React.FC<{}> = () => {
  const {
    prefixCls, checkOptions, tableDataSet, choseFieldStore,
    tableColumnCheckBoxesDataSet, issueFilterFormDataSet,
  } = useExportIssueStore();
  const [downloadInfo, setDownloadInfo] = useState({} as IDownLoadInfo);
  useEffect(() => {
    issueApi.loadLastImportOrExport('download_file').then((res: IDownLoadInfo) => {
      setDownloadInfo(res);
    });
  }, []);
  /**
 * 输出 excel
 */
  const exportExcel = async () => {
    // return;
    let search: any = {};
    if (await issueFilterFormDataSet.current?.validate()) {
      search = getCustomFieldFilters(choseFieldStore.getAllChosenField, issueFilterFormDataSet.current!);
    } else {
      return false;
    }
    if (await tableColumnCheckBoxesDataSet.current?.validate()) {
      if (!tableColumnCheckBoxesDataSet.current?.get('exportFieldCodes')
      || tableColumnCheckBoxesDataSet.current?.get('exportFieldCodes').length === 0) {
        Choerodon.prompt('请至少选择一个字段导出');
        return false;
      }
      search.exportFieldCodes = getExportFieldCodes(tableColumnCheckBoxesDataSet.current!);
    }
    const field = find(checkOptions, (f) => f.order) as { value: string, label: string, order?: string, };
    // @ts-ignore
    return issueApi.export(search, field ? `${field.name},${field.order}` : undefined)
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
  const handleFinish = (messageData: any) => {
    setDownloadInfo(messageData);
  };
  return (
    <div>
      <FormPart title="筛选问题" className={`${prefixCls}-form-filter`}>
        <IssueFilterForm dataSet={issueFilterFormDataSet} chosenFields={choseFieldStore.getAllChosenField} onDelete={(item) => choseFieldStore.delChosenFields(item.code)}>
          <ChooseField store={choseFieldStore} dropDownBtnProps={{ icon: 'add', style: { color: '#3f51b5' } }} />
        </IssueFilterForm>
      </FormPart>
      <Divider className={`${prefixCls}-horizontal`} />
      <FormPart title="选择字段" btnOnClick={handleChangeFieldStatus}>
        <TableColumnCheckBoxes options={checkOptions} dataSet={tableColumnCheckBoxesDataSet} name="exportFieldCodes" />
        <Button icon="unarchive" style={{ color: '#3f51b5' }} onClick={exportExcel}>导出问题</Button>
      </FormPart>
      <WsProgress
        messageKey="agile-export-issue"
        onFinish={handleFinish}
        autoDownload={{ fileName: `${getProjectName()}.xlsx` }}
        // visible
        downloadInfo={downloadInfo.id ? {
          url: downloadInfo.fileUrl!,
          lastUpdateDate: downloadInfo.lastUpdateDate!,
          createDate: downloadInfo.creationDate!,
        } : undefined}
      />
    </div>
  );
};

export default observer(ExportIssue);
