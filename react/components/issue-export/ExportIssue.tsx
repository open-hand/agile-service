import React, {
  memo, ReactElement, useEffect, useMemo, useState,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import { Divider } from 'choerodon-ui';
import classnames from 'classnames';
import { Button } from 'choerodon-ui/pro';
import IssueFilterForm, { useIssueFilterForm } from '@/components/issue-filter-form';
import ChooseField, { useChoseField } from '@/components/chose-field';
import TableColumnCheckBoxes, { useTableColumnCheckBoxes } from '@/components/table-column-check-boxes';
import WsProgress from '@/components/ws-progress';
import { getProjectName } from '@/utils/common';
import { useExportIssueStore } from './stores';
import { getCustomFieldFilters } from './utils';
import { IChosenFieldField } from '../chose-field/types';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
  children: ReactElement | ReactElement[] | null,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}
const FormPart: React.FC<FormPartProps> = memo((props) => {
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
            // color={'blue' as ButtonColor}
            onClick={handleClick}
          >
            {btnStatus !== 'NONE' ? '全选' : '全不选'}
          </Button>
        )}
      </div>
      <div className={`${prefixCls}-form-content`}>
        {children}
      </div>
    </div>
  );
});
interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}
const ExportIssue: React.FC = () => {
  const {
    prefixCls, checkOptions: propsCheckOptions, store, fields,
  } = useExportIssueStore();
  // 添加筛选配置 数据
  const [choseDataProps, choseComponentProps] = useChoseField({
    fields,
    defaultValue: store.getCurrentChosenFieldsArr,
    events: {
      initField: (data) => store.initField(data),
      initChosenField: (data) => store.initChosenField(data),
      choseField: (data) => handleChange(data),
    },
  });
  const { store: choseFieldStore } = choseDataProps;
  const checkOptions = useMemo(() => {
    const newCheckOptions = propsCheckOptions.concat([...(choseFieldStore.getOriginalField.get('custom') || [])].map((option) => ({ value: option.code, label: option.name })));
    return newCheckOptions;
  }, [choseFieldStore.getOriginalField, propsCheckOptions]);
  // 选择字段框配置 数据
  const [checkBoxDataProps, checkBoxComponentProps] = useTableColumnCheckBoxes({ options: checkOptions, defaultValue: store.defaultCheckedExportFields });

  const [filterData, filterComponentProps] = useIssueFilterForm({
    fields,
    value: choseFieldStore.getAllChosenField,
    extraFormItems: store.getExtraFields,
    systemDataSetField: store.dataSetSystemFields,
    extraRenderFields: store.renderField,
    events: {
      afterDelete: (item) => {
        choseFieldStore.delChosenFields(item.code);
      },
    },
  });
  const handleChange = (value: IChosenFieldField | IChosenFieldField[]) => {
    Array.isArray(value) ? value.forEach((v) => filterData.actions?.onAdd(v)) : filterData.actions?.onAdd(value);
  };
  useEffect(() => {
    store.loadRecordAxios(store).then((res: IDownLoadInfo) => {
      store.setDownloadInfo(res);
    });
  }, [store]);
  /**
 * 输出 excel
 */
  const exportExcel = async () => {
    let search: any = {};
    if (await filterData.dataSet.current?.validate()) {
      search = getCustomFieldFilters([...choseFieldStore.getAllChosenField, ...store.getExtraFields], filterData.dataSet.current!, store.transformSystemFilter);
    } else {
      return false;
    }
    if (checkBoxDataProps.checkedOptions.length === 0) {
      Choerodon.prompt('请至少选择一个字段导出');
      return false;
    }
    search.exportFieldCodes = store.transformExportFieldCodes(checkBoxDataProps.checkedOptions);
    search = store.exportBefore(search);
    const field = find(checkOptions, (f) => f.order) as { value: string, label: string, order?: string, };
    return store.exportAxios(search, field ? `${field.value},${field.order}` : undefined);
  };
  const handleChangeFieldStatus = (status: 'ALL' | 'NONE') => {
    if (status !== 'ALL') {
      checkBoxDataProps.actions.checkAll();
    } else {
      checkBoxDataProps.actions.unCheckAll();
    }
    return true;
  };
  const handleFinish = (messageData: any) => {
    store.setExportBtnHidden(false);
    store.setDownloadInfo(messageData);
  };
  const renderExport = () => {
    if (store.exportButtonConfig?.component) {
      return typeof (store.exportButtonConfig?.component) === 'function' ? store.exportButtonConfig?.component(exportExcel) : store.exportButtonConfig?.component;
    }
    return (
      <Button
        icon="unarchive"
        style={{ color: '#3f51b5' }}
        onClick={exportExcel}
        hidden={store.exportBtnHidden}
        {...store.exportButtonConfig?.buttonProps}
      >
        {store.exportButtonConfig?.buttonChildren ?? '导出问题'}
      </Button>
    );
  };
  return (
    <div>
      <FormPart title="筛选问题" className={`${prefixCls}-form-filter`}>
        <IssueFilterForm {...filterComponentProps}>
          <div style={{ marginTop: 4 }}>
            <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { color: '#3f51b5' } }} />
          </div>
        </IssueFilterForm>
      </FormPart>
      <Divider className={`${prefixCls}-horizontal`} />
      <FormPart title="选择字段" btnOnClick={handleChangeFieldStatus}>
        <TableColumnCheckBoxes {...checkBoxComponentProps} />
        {renderExport()}
      </FormPart>
      <WsProgress
        messageKey="agile-export-issue"
        onFinish={handleFinish}
        onStart={() => store.setExportBtnHidden(true)}
        autoDownload={{ fileName: `${getProjectName()}.xlsx` }}
        downloadInfo={store.downloadInfo.id ? {
          url: store.downloadInfo.fileUrl!,
          lastUpdateDate: store.downloadInfo.lastUpdateDate!,
          createDate: store.downloadInfo.creationDate!,
        } : undefined}
      />
    </div>
  );
};

export default observer(ExportIssue);
