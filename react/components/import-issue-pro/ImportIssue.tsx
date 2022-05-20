import React, {
  useRef, useEffect, useState,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import {
  Divider,
} from 'choerodon-ui';
import { observer } from 'mobx-react';
import { Button } from 'choerodon-ui/pro';
import FileSaver from 'file-saver';
import classnames from 'classnames';
import {
  includes, uniq, isEqualWith, intersection, sortBy,
} from 'lodash';
import { usePersistFn } from 'ahooks';
import ImportFields, { IImportIssueFieldsEvents, IIortIssueFieldsRef } from './ImportFields';
import TemplateSelect from '../template-select';
import SaveTemplateBtn from './SaveTemplateBtn';
import { useImportIssueContext } from './stores';
import WsProgress, { IWsProgressProps } from '../ws-progress';
import './ImportIssue.less';

function customizerFieldCodes(obj1: Record<string, any>, obj2: Record<string, any>) {
  if (Array.isArray(obj1) && Array.isArray(obj2)) {
    const newArr = intersection(obj1, obj2);
    return (newArr.length === obj1.length) && (newArr.length === obj2.length);
  }
  return undefined;
}
const ImportIssueForm = (formProps: any) => {
  const {
    title, children, bottom, className, contentStyle, titleStyle,
  } = formProps;
  const context = useImportIssueContext();
  const prefixCls = `${context.prefixCls}-form-one`;
  return (
    <div className={classnames(prefixCls, className)}>
      <div style={{
        display: 'flex',
        alignItems: 'center',
      }}
      >
        <span className={`${prefixCls}-block`} />
        <span className={`${prefixCls}-title`}>{title}</span>
      </div>
      <span className={`${prefixCls}-content`} style={contentStyle}>{children}</span>
      {bottom}
    </div>
  );
};

const ImportIssueContent: React.FC = observer(() => {
  const {
    action, modal, onSuccess, latestInfo, latestData,
    messageKey, applyType, name, requires, systems, fields: propsFields, importRequest, prefixCls, downloadTemplateRequest,
  } = useImportIssueContext();
  const [templateFirstLoaded, setTemplateFirstLoaded] = useState<boolean>(false);
  const [templateIsExist, setTemplateIsExist] = useState(false);
  const [allFields, setAllFields] = useState(() => [] as any[]);
  const [requiredFields, setRequiredFields] = useState([] as string[]);
  const [checkOptions, setCheckOptions] = useState([] as string[]);
  const [firstCheckOptions, setFirstCheckOptions] = useState<string[]>();
  latestData.allFields = allFields;
  latestData.requiredFields = requiredFields;
  latestData.checkOptions = checkOptions;
  const uploadInputRef = useRef<HTMLInputElement>(null);
  const importFieldsRef = useRef<IIortIssueFieldsRef>(null);
  const templateSelectRef = useRef<any>();
  const handleMessage = usePersistFn((data: any) => {
    if (data.status === 'success' || data.status === 'template_error_missing_required_column' || data.status === 'template_error' || data.status === 'empty_data_sheet' || data.status?.startsWith('error_custom_field_header')) {
      if (onSuccess && data?.successCount) {
        onSuccess();
      }
      return true;
    }
    return false;
  });

  const checkTemplateExist = usePersistFn((value: string[]) => {
    const templateList = templateSelectRef?.current?.templateList || [];
    const importFieldsData = { systemFields: [], customFields: [] } as any;
    const fields = uniq([...(value || []), ...latestData.requiredFields]);
    importFieldsData.systemFields = fields.filter((code) => latestData.allFields.find((item: any) => item.code === code && item.system)).sort();
    importFieldsData.customFields = fields.filter((code) => latestData.allFields.find((item: any) => item.code === code && !item.system)).sort();
    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqualWith(JSON.parse(templateList[i].templateJson), importFieldsData, customizerFieldCodes)) {
        setTemplateIsExist(true);
        templateSelectRef?.current?.setTemplate(templateList[i]);
        return;
      }
    }
    setTemplateIsExist(false);
    templateSelectRef?.current?.setTemplate(undefined);
  });
  const handleCheckBoxChangeOk: NonNullable<IImportIssueFieldsEvents['onUpdate']> = (value: string[], init) => {
    setCheckOptions(value);
    init ? setFirstCheckOptions(value) : setFirstCheckOptions(undefined);
    checkTemplateExist(value);
  };
  const selectTemplateOk = (fieldCodes: any[] | any) => {
    const newFields = Array.isArray(fieldCodes) ? fieldCodes : [...(fieldCodes.systemFields || []), ...(fieldCodes.customFields || [])];
    importFieldsRef.current?.setValue(newFields);
    checkTemplateExist(newFields);
  };
  // 模板加载完成后开始一次匹配
  useEffect(() => {
    if (templateFirstLoaded && firstCheckOptions && allFields.length && requiredFields) {
      checkTemplateExist(firstCheckOptions);
    }
  }, [checkTemplateExist, templateFirstLoaded, firstCheckOptions, allFields.length, requiredFields]);

  const upload = (file: FileList[0]) => {
    if (!file) {
      Choerodon.prompt('请选择文件');
      return;
    }
    const formData = new FormData();
    formData.append('file', file);
    importRequest(formData).then(() => {

    }).catch((e) => {
      Choerodon.prompt('网络错误');
    }).finally(() => {
      if (uploadInputRef.current) {
        uploadInputRef.current!.value = '';
      }
    });
  };
  const exportExcel = async () => {
    const importFieldsData = { systemFields: [] as string[], customFields: [] as string[] };
    const rankMap = new Map(allFields.map((item, index) => [item.code, index]));
    const rankCheckOptions = sortBy(checkOptions, (item) => rankMap.get(item) || 0);
    importFieldsData.systemFields = rankCheckOptions.filter((code: any) => allFields.find((item: any) => item.code === code && item.system));
    importFieldsData.customFields = rankCheckOptions.filter((code: any) => allFields.find((item: any) => item.code === code && !item.system));
    return downloadTemplateRequest(importFieldsData).then((excel) => {
      const blob = new Blob([excel], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
      const fileName = `${name || '工作项'}导入模板.xlsx`;
      FileSaver.saveAs(blob, fileName);
    });
  };
  const renderEndProgress: NonNullable<IWsProgressProps['renderEndProgress']> = (messageData) => {
    if (!messageData && !latestInfo) {
      return (
        <div>
          正在查询导入信息，请稍后
        </div>
      );
    }
    let {
      fileUrl,
      successCount,
      failCount,
    } = messageData || {};
    const { status } = messageData || {};
    if (status) {
      switch (status) {
        case 'failed':
          return (
            <div>
              <span className="c7n-importIssue-text">
                导入失败
                <span style={{ color: '#FF0000' }}>{failCount}</span>
                {`${name || '工作项'} `}
                <a href={fileUrl}>
                  点击下载失败详情
                </a>
              </span>
            </div>
          );
        case 'success':
          return (
            <div>
              <span className="c7n-importIssue-text">
                导入成功
                <span style={{ color: '#0000FF' }}>{successCount}</span>
                {`${name || '工作项'}`}
              </span>
            </div>
          );
        case 'template_error':
          return (
            <div>
              <span className="c7n-importIssue-text">
                导入模板错误，或无数据。
              </span>
            </div>
          );
        case 'empty_data_sheet':
          return (
            <div>
              <span className="c7n-importIssue-text">
                导入数据为空
              </span>
            </div>
          );
        case 'template_error_missing_required_column':
          return (
            <div>
              <span className="c7n-importIssue-text">
                模板不正确，缺少必要的列
              </span>
            </div>
          );
        default:
          break;
      }
      if (status.startsWith('error_custom_field_header')) {
        const msg = status.split('error_custom_field_header_')[1];
        return (
          <div>
            <span className="c7n-importIssue-text">
              {`自定义字段${msg}不存在`}
            </span>
          </div>
        );
      }
    }
    const {
      id,
    } = latestInfo || {};
    fileUrl = latestInfo?.fileUrl || fileUrl;
    successCount = latestInfo?.successCount || successCount || 0;
    failCount = latestInfo?.failCount || failCount || 0;
    return (
      <>
        {id && <Divider />}
        {id && (
          <ImportIssueForm
            title={`导入${name || '工作项'}`}
          >
            <div style={{ marginTop: 10 }}>
              上次导入共导入
              <span style={{ color: '#00bfa5', fontSize: 20, margin: '0 .04rem' }}>{successCount}</span>
              条数据成功,
              <span style={{ color: '#f76e64', fontSize: 20, margin: '0 .04rem' }}>{failCount}</span>
              条数据失败
              {fileUrl && (
                <a href={fileUrl}>
                  点击下载失败详情
                </a>
              )}
            </div>

          </ImportIssueForm>
        )}
      </>
    );
  };
  modal?.handleOk(async () => {
    uploadInputRef.current?.click();
    return false;
  });
  return (
    <div>
      {
        action && (
          <ImportIssueForm
            title="选择常用模板"
            bottom={null}
            className="c7n-importIssue-templateSelect"
          >
            <TemplateSelect
              templateSelectRef={templateSelectRef}
              action={action as any}
              checkOptions={allFields.map((item: any) => ({
                label: item.title,
                value: item.code,
                system: item.system,
                optionConfig: includes(requiredFields, item.code) ? {
                  disabled: includes(requiredFields, item.code),
                  defaultChecked: includes(requiredFields, item.code),
                  name: 'required-option',
                } : undefined,
              }))}
              selectTemplateOk={selectTemplateOk}
              transformExportFieldCodes={(data) => data}
              reverseTransformExportFieldCodes={(data) => data}
              defaultInitCodes={requiredFields}
              setTemplateFirstLoaded={setTemplateFirstLoaded}
            />
          </ImportIssueForm>
        )
      }
      <ImportIssueForm
        title="选择模板字段"
        className={classnames({ [`${prefixCls}-form-field`]: action })}
        contentStyle={{ marginTop: 12 }}
        bottom={(
          <>
            <Button
              type={'primary' as any}
              onClick={exportExcel}
              icon="get_app"
              className={`${prefixCls}-btn`}
            >
              下载模板
            </Button>
            {
              action && (
                <SaveTemplateBtn
                  action={action as any}
                  templateSelectRef={templateSelectRef}
                  templateIsExist={templateIsExist}
                />
              )
            }
          </>
        )}
      >
        {`您必须使用模板文件，录入${name || '工作项'}信息。`}
        <ImportFields
          ref={importFieldsRef}
          requires={requires}
          applyType={applyType as any}
          systems={systems}
          fields={propsFields}
          events={{
            onOptionLoad: setAllFields,
            onRequiredFieldLoad: setRequiredFields,
            onUpdate: handleCheckBoxChangeOk,
          }}
        />
      </ImportIssueForm>

      <input
        ref={uploadInputRef}
        type="file"
        onChange={(e) => {
          if (e.target.files && e.target.files[0]) {
            upload(e.target.files[0]);
          }
        }}
        style={{ display: 'none' }}
        accept="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      />
      <WsProgress
        predefineProgressTextConfig="import"
        handleMessage={handleMessage}
        onStart={() => modal?.update({ okProps: { loading: true } })}
        onFinish={() => modal?.update({ okProps: { loading: false } })}
        onFailed={() => modal?.update({ okProps: { loading: false } })}
        messageKey={messageKey}
        renderEndProgress={renderEndProgress}
      />
    </div>
  );
});
export default ImportIssueContent;
