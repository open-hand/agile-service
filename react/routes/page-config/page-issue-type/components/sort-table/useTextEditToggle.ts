import React, {
  useCallback, useMemo, useRef,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Action } from '@/components/TextEditTogglePro/TextEditToggle';
import renderEditor from '../../../components/renderEditor';
import {
  transformDefaultValue, orgDisabledEditDefaultFields, disabledEditDefaultFields,
} from '../../utils';

interface ITextEditToggleConfigProps {
  key: string
  submitTrigger: Action[] // 触发提交的动作
  alwaysRender: boolean
  editor: () => JSX.Element
  editorExtraContent?: () => JSX.Element
  // children: JSX.Element
  className?: string
  disabled?: boolean
  onSubmit: (data: any) => void
  initValue: any
}

/**
 *
 * @param record
 */
function useTextEditTogglePropsWithPage(record: Record, isProject: boolean, { className }: { className?: string }): ITextEditToggleConfigProps {
  const fieldType = record.get('fieldType');
  const dataRef = useRef<Array<any> | undefined>();
  const handleSubmit = useCallback((value: any) => {
    const currentData = record.toData();
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
      if (!value && value === null) {
        record.set('localDefaultObj', undefined);
        currentDefaultValueObj = undefined;
      }
    }
    if (['date', 'datetime', 'time'].includes(fieldType)) {
      const { meaning, value: dateValue } = newValue || {};
      newValue = dateValue === 'current' ? currentData.defaultValue || meaning : value;
      record.set('extraConfig', dateValue === 'current');
    }

    record.set('defaultValue', newValue);
    record.set('showDefaultValueText', transformDefaultValue({
      ...currentData,
      // @ts-ignore
      optionKey: currentData.localSource === 'created' ? 'tempKey' : 'id',
      defaultValue: newValue,
      defaultValueObj: currentDefaultValueObj,
      fieldOptions: currentData.fieldOptions || dataRef.current,
    }));
  }, [fieldType, record]);
  const initValue = useMemo(() => {
    if (['date', 'datetime', 'time'].includes(fieldType) && record.get('extraConfig')) {
      return 'current';
    }
    return typeof (record.get('defaultValue')) === 'undefined' || record.get('defaultValue') === '' ? undefined : record.get('defaultValue');
  }, [fieldType, record, record.get('defaultValue'), record.get('extraConfig')]);
  const variableProps = useMemo(() => {
    let editor = () => renderEditor({
      data: {
        fieldType,
        fieldCode: record.get('fieldCode'),
        defaultValue: initValue,
        extraConfig: record.get('extraConfig'),
        fieldOptions: record.get('fieldOptions'),
      },
      dataRef,
      dropdownMatchSelectWidth: false,
      dropdownMenuStyle: { minWidth: 165, maxWidth: 300 },
    });
    if (fieldType === 'member' || fieldType === 'multiMember') {
      const defaultValueObj = record.get('defaultValueObjs') || record.get('defaultValueObj') || record.get('localDefaultObj') || undefined;
      editor = () => renderEditor({
        data: {
          fieldType, fieldCode: record.get('fieldCode'), defaultValue: defaultValueObj, extraConfig: record.get('extraConfig'), fieldOptions: record.get('fieldOptions'),
        },
        dataRef,
        dropdownMatchSelectWidth: false,
        dropdownMenuStyle: { minWidth: 165, maxWidth: 300 },
      });
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
    const submitOnChange = ['member', 'single', 'radio', 'date', 'datetime', 'time'].includes(fieldType);
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
    // if (isProject && record.get('createdLevel') === 'organization') {
    //   return true;
    // }
    if (record.get('createdLevel') === 'system') {
      return isProject ? disabledEditDefaultFields.includes(record.get('fieldCode')) : orgDisabledEditDefaultFields.includes(record.get('fieldCode'));
    }
    return false;
  }, [isProject, record]);
  return {
    ...variableProps,
    ...constantProps,
    disabled,
    className: !disabled ? className : undefined,
  };
}
export default useTextEditTogglePropsWithPage;
export { disabledEditDefaultFields, orgDisabledEditDefaultFields };
