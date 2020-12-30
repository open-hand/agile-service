import React, {
  useCallback, useMemo, useState,
} from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Action } from '@/components/TextEditTogglePro/TextEditToggle';
import renderEditor from './renderEditor';
import { transformDefaultValue } from '../../utils';

interface ITextEditToggleConfigProps {
  key: string
  submitTrigger: Action[] // 触发提交的动作
  alwaysRender: boolean
  editor: () => JSX.Element
  editorExtraContent?: () => JSX.Element
  // children: JSX.Element
  // className?: string
  onSubmit: (data: any) => void
  initValue: any
}
/**
 *
 * @param record
 */
function useTextEditTogglePropsWithPage(record: Record): ITextEditToggleConfigProps {
  const fieldType = record.get('fieldType');
  const handleSubmit = useCallback((data) => {
    console.log('data', data, record.toData());
    // const { };
    const local = record.get('local');
    record.set('defaultValue', data);

    // switch (fieldType) {
    //   case 'input':
    //   case 'member':
    //   case 'text': {
    //     record.set('defaultValue', data);
    //     break;
    //   }

    //   default:
    //     break;
    // }
    const currentData = record.toData();
    console.log('show txt', transformDefaultValue(currentData));
    record.set('showDefaultValueText', transformDefaultValue(currentData));
  }, [record]);
  const variableProps = useMemo(() => {
    let defaultValue = record.get('defaultValue');
    let editor = () => renderEditor({ record, defaultValue });
    if (fieldType === 'member') {
      const defaultValueObj = record.get('defaultValueObj') || record.get('localDefaultObj') || {};
      defaultValue = defaultValueObj.id;
      editor = () => renderEditor({ record, defaultValue: defaultValueObj });
    }
    return {
      initValue: defaultValue,
      editor,
    };
  }, [fieldType, record]);
  const constantProps = useMemo(() => {
    const key = `page-issue-type-default-edit-text-${record.id}`;
    const submitTrigger = ['change', 'blur'] as Action[];
    return {
      alwaysRender: false,
      submitTrigger,
      onSubmit: handleSubmit,
      key,
    };
  }, [handleSubmit, record.id]);
  console.log(`generate config for ${fieldType}`);
  return {
    ...variableProps,
    ...constantProps,
  };
}
export default useTextEditTogglePropsWithPage;
