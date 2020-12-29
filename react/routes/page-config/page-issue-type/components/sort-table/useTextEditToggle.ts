import React, {
  useCallback, useMemo, useState,
} from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Action } from '@/components/TextEditTogglePro/TextEditToggle';
import renderEditor from './renderEditor';

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

function useTextEditTogglePropsWithPage(record: Record): ITextEditToggleConfigProps {
  const fieldType = record.get('fieldType');
  const handleSubmit = useCallback((data) => {
    console.log('data', data, record.toData());
    // const local = record.get('local');
    // switch (fieldType) {
    //   case 'input':
    //   case 'text': {
    //     record.set('defaultValue', data);

    //     return
    //   }

    //   default:
    //     break;
    // }
  }, [record]);
  const variableProps = useMemo(() => {
    console.log('render.', fieldType);
    const defaultValue = record.get('localDefaultValue') || record.get('defaultValue');

    return {
      initValue: defaultValue,
      onSubmit: handleSubmit,
    };
  }, [fieldType, handleSubmit, record]);
  const editor = useMemo(() => () => renderEditor({ record }), [record]);
  const constantProps = useMemo(() => {
    const key = `page-issue-type-default-edit-text-${record.id}`;
    const submitTrigger = ['change', 'blur'] as Action[];
    return {
      alwaysRender: false,
      submitTrigger,
      key,
    };
  }, [record.id]);
  return {
    editor,
    ...variableProps,
    ...constantProps,
  };
}
export default useTextEditTogglePropsWithPage;
