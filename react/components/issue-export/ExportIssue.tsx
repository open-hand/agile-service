import React, {
  memo, ReactElement, useCallback, useEffect, useMemo, useState, useRef,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import {
  find, findIndex, isEqual, map, uniq, uniqBy,
} from 'lodash';
import classnames from 'classnames';
import { Button } from 'choerodon-ui/pro';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import IssueFilterForm, { useIssueFilterForm } from '@/components/issue-filter-form';
import ChooseField, { useChoseField } from '@/components/chose-field';
import TableColumnCheckBoxes, { ITableColumnCheckBoxesDataProps, useTableColumnCheckBoxes } from '@/components/table-column-check-boxes';
import WsProgress from '@/components/ws-progress';
import { getProjectName, getProjectId } from '@/utils/common';
import { useExportIssueStore } from './stores';
import { getCustomFieldFilters } from './utils';
import { IChosenFieldField } from '../chose-field/types';
import TemplateSelect from '../template-select/TemplateSelect';
import openSaveTemplate from '../template-select/components/save/SaveTemplate';
import { ITemplate } from '../template-select/components/edit/EditTemplate';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
  children: ReactElement | ReactElement[] | null | Array<ReactElement | null>,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
  titleLine?: boolean
}
export const FormPart: React.FC<FormPartProps> = memo((props) => {
  const {
    title, children, btnOnClick, titleLine = true,
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
        {titleLine ? <div className={`${prefixCls}-form-block`} /> : null}
        <span>{title}</span>
        {!!btnOnClick && (
          <Button
            className={`${prefixCls}-form-btn`}
            funcType={'flat' as FuncType}
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
  const [templateIsExist, setTemplateIsExist] = useState(false);
  const templateSelectRef = useRef<{
    onOk:(template: ITemplate) => Promise<void>,
    templateList: ITemplate[]
    setTemplate: (template: ITemplate | undefined) => void
    templateFirstLoaded: boolean
  }>();

  const checkBoxDataPropsRef = useRef<ITableColumnCheckBoxesDataProps>();
  const selectTemplateOkRef = useRef<(codes: string[]) => void>();

  const {
    prefixCls, checkOptions: propsCheckOptions, store, fields, modal, action, exportBtnText,
  } = useExportIssueStore();

  // 添加筛选配置 数据
  const [choseDataProps, choseComponentProps] = useChoseField({
    fields,
    defaultValue: store.getCurrentChosenFieldsArr,
    events: {
      initField: (data) => store.initField(data),
      initFieldFinish: (cField, sField, current) => store.initFieldFinish(cField, sField, current),
      initChosenField: (data) => store.initChosenField(data),
      choseField: (data) => handleChange(data),
    },
  });

  const { store: choseFieldStore } = choseDataProps;
  const checkOptions = useMemo(() => { // checkBokProps
    const newCheckOptions = propsCheckOptions.map((option) => ({ ...option, ...store.checkboxOptionsExtraConfig?.get(option.value) })) || [];
    newCheckOptions.push(...(choseFieldStore.getOriginalField.get('custom') || []).map((option) => ({ value: option.code, label: option.name, ...store.checkboxOptionsExtraConfig?.get(option.code) })));
    return uniqBy(newCheckOptions, 'value');
  }, [choseFieldStore.getOriginalField, propsCheckOptions, store.checkboxOptionsExtraConfig]);

  const handleCheckBoxChangeOk = useCallback((value) => {
    const currentFieldCodes = store.transformExportFieldCodes(value, checkBoxDataPropsRef?.current);
    const reverseFieldCodes = store.reverseTransformExportFieldCodes(uniq(currentFieldCodes)).filter((code) => map(checkOptions, 'value').includes(code));

    const templateList = templateSelectRef?.current?.templateList || [];
    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqual(JSON.parse(templateList[i].templateJson).sort(), store.transformExportFieldCodes(reverseFieldCodes, checkBoxDataPropsRef?.current).sort())) {
        templateSelectRef?.current?.setTemplate(templateList[i]);
        return;
      }
    }
    templateSelectRef?.current?.setTemplate(undefined);
  }, [checkOptions, store]);

  useEffect(() => {
    if (templateSelectRef?.current?.templateFirstLoaded) {
      handleCheckBoxChangeOk(store.defaultCheckedExportFields);
    }
  }, [handleCheckBoxChangeOk, store.defaultCheckedExportFields, templateSelectRef?.current?.templateFirstLoaded]);

  // 选择字段框配置 数据
  const [checkBoxDataProps, checkBoxComponentProps] = useTableColumnCheckBoxes({
    options: checkOptions,
    defaultValue: store.defaultCheckedExportFields,
    events: { initOptions: store.defaultInitOptions },
    onChange: handleCheckBoxChangeOk,
  });

  Object.assign(checkBoxDataPropsRef, {
    current: checkBoxDataProps,
  });

  const [filterData, filterComponentProps] = useIssueFilterForm({
    fields,
    value: choseFieldStore.getAllChosenField,
    defaultValue: store.getCurrentChosenFieldsArr,
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
  const exportExcel = useCallback(async () => {
    let search: any = {};
    if (await filterData.dataSet.current?.validate()) {
      search = getCustomFieldFilters([...choseFieldStore.getAllChosenField, ...store.getExtraFields], filterData.dataSet.current!, store.transformSystemFilter);
    } else {
      return false;
    }
    search.exportFieldCodes = store.transformExportFieldCodes(checkBoxDataProps.checkedOptions.sort((a, b) => findIndex(checkOptions, { value: a }) - findIndex(checkOptions, { value: b })), checkBoxDataProps);
    if (checkBoxDataProps.checkedOptions.length === 0) {
      Choerodon.prompt('请至少选择一个字段导出');
      return false;
    }
    search = store.exportBefore(search);
    const field = find(checkOptions, (f) => f.order) as { value: string, label: string, order?: string, };
    if (store.exportButtonConfig?.buttonProps) {
      store.exportButtonConfig.buttonProps.loading = true;
    } else if (store.exportButtonConfig && !store.exportButtonConfig.buttonProps) {
      store.exportButtonConfig.buttonProps = {
        loading: true,
      };
    }
    store.exportAxios(search, field ? `${field.value},${field.order}` : undefined);
    return false;
  }, [checkBoxDataProps, checkOptions, choseFieldStore.getAllChosenField, filterData.dataSet, store]);

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
    // @ts-ignore
    store.exportButtonConfig.buttonProps.loading = false;
    modal?.update({ okProps: { loading: false } });
    Choerodon.prompt('导出成功');
    store.setDownloadInfo(messageData);
  };
  const renderExport = () => {
    if (store.exportButtonConfig?.component) {
      return typeof (store.exportButtonConfig?.component) === 'function' ? store.exportButtonConfig?.component(exportExcel) : store.exportButtonConfig?.component;
    }
    return null;
  };

  const handleSaveTemplate = useCallback(() => {
    // @ts-ignore
    openSaveTemplate({ action, onOk: templateSelectRef.current?.onOk, templateJson: JSON.stringify(store.transformExportFieldCodes(checkBoxDataProps.checkedOptions, checkBoxDataProps)) });
  }, [action, checkBoxDataProps, store]);

  const selectTemplateOk = useCallback((fieldCodes) => {
    const newCheckedOptions = store.reverseTransformExportFieldCodes(uniq(fieldCodes)).filter((code) => map(checkOptions, 'value').includes(code));
    checkBoxDataProps.dataSet.current?.set('exportCodes', newCheckedOptions);
    checkBoxDataProps.setCheckedOptions(newCheckedOptions);
  }, [checkBoxDataProps, checkOptions, store]);

  Object.assign(selectTemplateOkRef, {
    current: selectTemplateOk,
  });
  useEffect(() => {
    modal?.handleOk(exportExcel);
  }, [exportExcel, modal]);
  useEffect(() => {
    const currentFieldCodes = store.transformExportFieldCodes(checkBoxDataProps.checkedOptions, checkBoxDataProps);
    if (!currentFieldCodes.length) { // 没有字段选中时不应该显示保存按钮
      setTemplateIsExist(true);
      return;
    }
    const reverseFieldCodes = store.reverseTransformExportFieldCodes(uniq(currentFieldCodes)).filter((code) => map(checkOptions, 'value').includes(code));

    const templateList = templateSelectRef?.current?.templateList || [];
    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqual(JSON.parse(templateList[i].templateJson).sort(), store.transformExportFieldCodes(reverseFieldCodes, checkBoxDataProps).sort())) {
        setTemplateIsExist(true);
        return;
      }
    }
    setTemplateIsExist(false);
  }, [checkBoxDataProps, checkOptions, store]);

  return (
    <div>
      <FormPart title="筛选工作项" className={`${prefixCls}-form-filter`}>
        <IssueFilterForm {...filterComponentProps}>
          <div style={{ marginTop: 4 }}>
            <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { marginLeft: 6, marginTop: 10 } }} />
          </div>
        </IssueFilterForm>
      </FormPart>
      {
        action && (
          <>
            <FormPart title="选择常用模板" className={`${prefixCls}-form-template`}>
              <TemplateSelect
                templateSelectRef={templateSelectRef}
                action={action}
                // @ts-ignore
                checkOptions={checkOptions}
                selectTemplateOk={selectTemplateOk}
                transformExportFieldCodes={store.transformExportFieldCodes}
                reverseTransformExportFieldCodes={store.reverseTransformExportFieldCodes}
              />
            </FormPart>
          </>
        )
      }
      <FormPart title="选择模板字段" btnOnClick={handleChangeFieldStatus}>
        <TableColumnCheckBoxes {...checkBoxComponentProps} />
        {renderExport()}
      </FormPart>
      <div className={`${prefixCls}-btns`}>
        <Button
          icon="unarchive-o"
          onClick={exportExcel}
          className="c7n-exportIssue-btn"
          loading={store.exportButtonConfig?.buttonProps?.loading}
        >
          导出
        </Button>
        {
          !templateIsExist && (
            <Button
              icon="unarchive"
              onClick={handleSaveTemplate}
              className="c7n-exportIssue-btn"
              style={{
                marginLeft: 20,
              }}
            >
              保存为常用模板
            </Button>
          )
        }
      </div>
      <WsProgress
        className={`${prefixCls}-wsProgress-area`}
        messageKey={`agile-export-issue-${getProjectId()}`}
        onFinish={handleFinish}
        onStart={() => {
            modal?.update({ okProps: { loading: true } });
            store.setExportBtnHidden(true);
        }}
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
