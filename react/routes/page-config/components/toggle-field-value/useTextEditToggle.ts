import {
  useCallback, useMemo, useRef,
} from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import { Action } from '@/components/TextEditTogglePro/TextEditToggle';
import { getMenuType } from '@/utils/common';
import renderEditor from '../renderEditor';
import {
  transformDefaultValue, orgDisabledEditDefaultFields, disabledEditDefaultFields,
} from '../../page-issue-type/utils';

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
function useTextEditTogglePropsWithPage(record: Record): ITextEditToggleConfigProps {
  const fieldType = record.get('fieldType');
  const isProject = useMemo(() => getMenuType() === 'project', []);
  const dataRef = useRef<Array<any> | undefined>();
  const handleSubmit = useCallback((value: any) => {
    const currentData = record.toData();
    let newValue = value;
    let extraConfig: boolean | undefined;
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
      extraConfig = dateValue === 'current';
      // newValue = currentData.defaultValue;
      newValue = extraConfig ? currentData.defaultValue || moment().format('YYYY-MM-DD HH:mm:ss') : dateValue;
      extraConfig && record.init('defaultValue', newValue);
      record.set('extraConfig', extraConfig);
    }
    record.set('defaultValue', newValue);
    record.set('showDefaultValueText', transformDefaultValue({
      ...currentData,
      extraConfig,
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
    return typeof (record.get('defaultValue')) === 'undefined' || record.get('defaultValue') === '' || record.get('defaultValue') === null ? undefined : record.get('defaultValue');
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
    // if (submitOnOut) {
    //   submitTrigger.push('click');
    // }
    return {
      alwaysRender: record.get('fieldCode') === 'product',
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
  };
}
export default useTextEditTogglePropsWithPage;
export { disabledEditDefaultFields, orgDisabledEditDefaultFields };
