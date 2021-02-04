import React, { useCallback, useEffect, useState } from 'react';
import { Button, DataSet } from 'choerodon-ui/pro';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { uniq } from 'lodash';
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
    allFields: { title: string, code: string, system: boolean}[],
    requiredFields: string[]
    chooseDataSet: DataSet
  }>,
  templateSelectRef: React.MutableRefObject<{
    onOk: (template: ITemplate) => Promise<void>,
    templateList: ITemplate[],
    setTemplate: (template: ITemplate | undefined) => void
    templateFirstLoaded: boolean,
  } | undefined>
  checkBoxChangeOk: (data: string[]) => void
  templateIsExist: boolean
}

const SaveTemplateBtn: React.FC<Props> = ({
  importFieldsRef, templateSelectRef, checkBoxChangeOk, templateIsExist,
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
      action: 'agile_import_issue',
      // @ts-ignore
      onOk: templateSelectRef.current?.onOk,
      templateJson: JSON.stringify(importFieldsData),
    });
  }, [importFieldsRef, templateSelectRef]);

  useEffect(() => {
    if (templateSelectRef?.current?.templateFirstLoaded) {
      checkBoxChangeOk(importFieldsRef?.current?.requiredFields || []);
    }
  }, [checkBoxChangeOk, importFieldsRef, importFieldsRef?.current?.requiredFields, templateSelectRef, templateSelectRef?.current?.templateFirstLoaded]);

  return (
    <>
      {
        !templateIsExist && (
        <Button
          icon="unarchive"
          funcType={'flat' as FuncType}
          color={'primary' as ButtonColor}
          onClick={handleSaveTemplate}
          style={{
            marginTop: -6,
            marginLeft: 5,
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
