import React, {
  useCallback, useMemo, useRef, useState,
} from 'react';
import { set } from 'lodash';
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
  const initValue = record.get('defaultValue');
  const dataRef = useRef<Array<any> | undefined>();
  const handleSubmit = useCallback((data) => {
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
    record.set('showDefaultValueText', transformDefaultValue(currentData));
  }, [record]);
  const variableProps = useMemo(() => {
    let defaultValue = record.get('defaultValue');
    let editor = () => renderEditor({ record, defaultValue });
    if (fieldType === 'member') {
      const defaultValueObj = record.get('defaultValueObj') || record.get('localDefaultObj') || {};
      defaultValue = defaultValueObj.id;
      editor = () => renderEditor({ record, defaultValue: defaultValueObj, dataRef });
    }
    return {
      initValue: defaultValue,
      editor,
    };
  }, [fieldType, record, record.get('defaultValue'), record.get('defaultValueObj'), record.get('localDefaultObj')]);
  const constantProps = useMemo(() => {
    const key = `page-issue-type-default-edit-text-${record.id}`;
    // const submitTrigger = ['click', 'change'] as Action[]; // 'change', 'blur'
    const submitTrigger = ['blur'] as Action[];
    const submitOnChange = ['member', 'single', 'radio'].includes(fieldType);
    if (submitOnChange) {
      submitTrigger.push('change');
    }
    const submitOnOut = ['radio'].includes(fieldType);
    if (submitOnOut) {
      submitTrigger.push('click');
    }
    return {
      alwaysRender: false,
      submitTrigger,
      onSubmit: (value: any) => {
        const currentData = record.toData();
        let currentDefaultValueObj = currentData.localDefaultObj || currentData.defaultValueObj;
        if (fieldType === 'member') {
          const newLocalDefaultObj = dataRef.current?.find((item) => item.id === value);
          if (newLocalDefaultObj && currentData.defaultValueObj && newLocalDefaultObj.id === currentData.defaultValueObj.id) {
            record.getField('localDefaultObj')?.reset();
            currentDefaultValueObj = currentData.defaultValueObj;
            // record.set('localDefaultObj', undefined);
          } else if (newLocalDefaultObj) {
            record.set('localDefaultObj', newLocalDefaultObj);
            currentDefaultValueObj = newLocalDefaultObj;
          }
        }
        record.set('defaultValue', value);
        // console.log('constantProps... onSubmit', currentDefaultValueObj, currentData);
        record.set('showDefaultValueText', transformDefaultValue({
          ...currentData,
          defaultValue: value,
          defaultValueObj: currentDefaultValueObj,
        }));
      },
      key,
    };
  }, [record]);
  return {
    ...variableProps,
    ...constantProps,
    initValue,
  };
}
export default useTextEditTogglePropsWithPage;
