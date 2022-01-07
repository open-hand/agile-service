import React, { useCallback, useEffect, useRef } from 'react';
import { Button, DataSet } from 'choerodon-ui/pro';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { uniq } from 'lodash';
import { TemplateAction } from '@/api';
import openSaveTemplate from '../template-select/components/save/SaveTemplate';
import { ITemplate } from '../template-select/components/edit/EditTemplate';

export const transformTemplateJson = (templateJson: string) => {
  const templateParse = JSON.parse(templateJson);
  return ({
    systemFields: (templateParse.systemFields || []).sort(),
    customFields: (templateParse.customFields || []).sort(),
  });
};

interface Props {
  importFieldsRef: React.MutableRefObject<{
    fields: string[]
    allFields: { title: string, code: string, system: boolean }[],
    requiredFields: string[]
    chooseDataSet: DataSet
  }>,
  templateSelectRef: React.MutableRefObject<{
    onOk: (template: ITemplate) => Promise<void>,
    templateList: ITemplate[],
    setTemplate: (template: ITemplate | undefined) => void
  } | undefined>
  checkBoxChangeOk: (data: string[]) => void
  templateIsExist: boolean
  action: TemplateAction
  templateFirstLoaded: boolean
}

const SaveTemplateBtn: React.FC<Props> = ({
  importFieldsRef, templateSelectRef, checkBoxChangeOk, templateIsExist, action, templateFirstLoaded,
}) => {
  const handleSaveTemplate = useCallback(() => {
    const importFieldsData: {
      systemFields: string[]
      customFields: string[]
    } = { systemFields: [], customFields: [] };

    const allFields = importFieldsRef.current?.allFields || [];
    const fields = uniq([...(importFieldsRef.current?.fields || []), ...(importFieldsRef.current?.requiredFields || [])]);
    importFieldsData.systemFields = fields.filter((code: string) => allFields.find((item) => item.code === code && item.system));
    importFieldsData.customFields = fields.filter((code: string) => allFields.find((item) => item.code === code && !item.system));
    openSaveTemplate({
      action,
      // @ts-ignore
      onOk: templateSelectRef.current?.onOk,
      templateJson: JSON.stringify(importFieldsData),
    });
  }, [action, importFieldsRef, templateSelectRef]);

  useEffect(() => {
    // 默认必填字段检查 是否为保存的模块
    // TODO 此处的检查函数后续需要更换
    if (templateFirstLoaded && importFieldsRef.current?.requiredFields?.length && importFieldsRef.current?.allFields?.length) {
      checkBoxChangeOk(importFieldsRef?.current?.requiredFields);
    }
  }, [checkBoxChangeOk, importFieldsRef, importFieldsRef.current?.requiredFields, importFieldsRef.current?.allFields, templateFirstLoaded, templateSelectRef]);

  return (
    <>
      {
        !templateIsExist && (
          <Button
            icon="unarchive"
            onClick={handleSaveTemplate}
            className="c7n-importIssue-btn"
            style={{
              marginLeft: 16,
            }}
          >
            保存为常用模板
          </Button>
        )
      }
    </>
  );
};

export default SaveTemplateBtn;
