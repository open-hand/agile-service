import React, {
  useCallback, useMemo, useRef, useState,
} from 'react';
import { set } from 'lodash';
import moment from 'moment';
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
  disabled?: boolean
  onSubmit: (data: any) => void
  initValue: any
}
const disabledEditDefaultFields = ['featureType', 'issueType', 'status', 'priority', 'creationDate', 'lastUpdateDate', 'timeTrace', 'belongToBacklog', 'urgent', 'progressFeedback'];
const orgDisabledEditDefaultFields = [...disabledEditDefaultFields, 'component', 'label', 'influenceVersion', 'fixVersion', 'epic', 'sprint', 'pi', 'subProject'];
const fieldTextValueConfig = {
  epic: { optionKey: 'issueId', textKey: 'epicName' },
};
/**
 *
 * @param record
 */
function useTextEditTogglePropsWithPage(record: Record, isProject: boolean): ITextEditToggleConfigProps {
  const fieldType = record.get('fieldType');
  const dataRef = useRef<Array<any> | undefined>();
  const handleSubmit = useCallback((value: any) => {
    const currentData = record.toData();
    console.log('current onSubmit', fieldTextValueConfig[currentData.fieldCode as keyof typeof fieldTextValueConfig]);

    let newValue = value;
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
    if (['date', 'datetime', 'time'].includes(fieldType)) {
      console.log('value', value, value === 'current');
      newValue = value === 'current' ? currentData.defaultValue : value;
      record.set('extraConfig', value === 'current');
    }

    record.set('defaultValue', newValue);
    console.log('dataRef.current', dataRef.current);
    record.set('showDefaultValueText', transformDefaultValue({
      ...currentData,
      // @ts-ignore
      optionKey: currentData.localSource === 'created' ? 'tempKey' : 'id',
      defaultValue: newValue,
      defaultValueObj: currentDefaultValueObj,
      fieldOptions: currentData.fieldOptions || dataRef.current,
      ...fieldTextValueConfig[currentData.fieldCode as keyof typeof fieldTextValueConfig],
    }));
  }, [fieldType, record]);
  const initValue = useMemo(() => {
    if (['date', 'datetime', 'time'].includes(fieldType) && record.get('extraConfig')) {
      return 'current';
    }
    return typeof (record.get('defaultValue')) === 'undefined' || record.get('defaultValue') === '' ? undefined : record.get('defaultValue');
  }, [fieldType, record, record.get('defaultValue'), record.get('extraConfig')]);
  const variableProps = useMemo(() => {
    let editor = () => renderEditor({ record, defaultValue: initValue, dataRef });
    if (fieldType === 'member') {
      const defaultValueObj = record.get('defaultValueObj') || record.get('localDefaultObj') || {};
      editor = () => renderEditor({ record, defaultValue: defaultValueObj, dataRef });
    }
    return {
      initValue,
      editor,
    };
  }, [fieldType, initValue, record]);
  const constantProps = useMemo(() => {
    const key = `page-issue-type-default-edit-text-${record.id}`;
    // const submitTrigger = ['click', 'change'] as Action[]; // 'change', 'blur'
    const submitTrigger = ['blur'] as Action[];
    const submitOnChange = ['member', 'single', 'radio'].includes(fieldType);
    if (submitOnChange) {
      submitTrigger.push('change');
    }
    const submitOnOut = ['radio'].includes(fieldType);
    // if (submitOnOut) {
    //   submitTrigger.push('click');
    // }
    return {
      alwaysRender: false,
      submitTrigger,
      onSubmit: handleSubmit,
      key,
    };
  }, [fieldType, handleSubmit, record.id]);
  const disabled = useMemo(() => {
    if (isProject && record.get('createdLevel') === 'organization') {
      return true;
    }
    if (record.get('createdLevel') === 'system') {
      return isProject ? disabledEditDefaultFields.includes(record.get('fieldCode')) : orgDisabledEditDefaultFields.includes(record.get('fieldCode'));
    }
    return false;
  }, [isProject, record]);
  return {
    ...variableProps,
    ...constantProps,
    disabled,

  };
}
export default useTextEditTogglePropsWithPage;
