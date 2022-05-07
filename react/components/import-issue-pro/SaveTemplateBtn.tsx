import React, {
  MutableRefObject, useCallback, useEffect, useRef,
} from 'react';
import { Button, DataSet } from 'choerodon-ui/pro';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { uniq } from 'lodash';
import { TemplateAction } from '@/api';
import openSaveTemplate from '../template-select/components/save/SaveTemplate';
import { ITemplate } from '../template-select/components/edit/EditTemplate';
import { useImportIssueContext } from './stores';

export const transformTemplateJson = (templateJson: string) => {
  const templateParse = JSON.parse(templateJson);
  return ({
    systemFields: (templateParse.systemFields || []).sort(),
    customFields: (templateParse.customFields || []).sort(),
  });
};

interface Props {
  templateSelectRef: React.MutableRefObject<{
    onOk: (template: ITemplate) => Promise<void>,
    templateList: ITemplate[],
    setTemplate: (template: ITemplate | undefined) => void
  } | undefined>
  templateIsExist: boolean
  action: TemplateAction
}

const SaveTemplateBtn: React.FC<Props> = ({
  templateSelectRef, templateIsExist, action,
}) => {
  const { latestData } = useImportIssueContext();
  const handleSaveTemplate = useCallback(() => {
    const importFieldsData: {
      systemFields: string[]
      customFields: string[]
    } = { systemFields: [], customFields: [] };
    const allFields = latestData.allFields || [];
    const fields = uniq([...(latestData.checkOptions || []), ...(latestData.requiredFields || [])]);
    importFieldsData.systemFields = fields.filter((code: string) => allFields.find((item) => item.code === code && item.system));
    importFieldsData.customFields = fields.filter((code: string) => allFields.find((item) => item.code === code && !item.system));
    openSaveTemplate({
      action,
      // @ts-ignore
      onOk: templateSelectRef.current?.onOk,
      templateJson: JSON.stringify(importFieldsData),
    });
  }, [action, latestData, templateSelectRef]);

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
