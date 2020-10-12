import React, {
  useRef, useCallback, useMemo, useImperativeHandle,
} from 'react';
import { Form, Button } from 'choerodon-ui/pro';
import { find } from 'lodash';
import SelectField from '@/components//field/select-field';
import Field from '@/components/field';
import useFilter, { ISystemField, ICustomField, IFilterField } from './useFilter';

export interface IFilter {
  [key: string]: any
}
interface FilterProps {
  selected: string[]
  value: IFilter
  innerRef?: React.MutableRefObject<{
    validate: () => Promise<boolean>
  }>,
  onFilterChange: (code: string, value: any) => void
  onSelectChange: (code: string | string[], select: boolean) => void
  systemFields: ISystemField[]
  customFields: ICustomField[]
}
const Filter: React.FC<FilterProps> = ({
  systemFields,
  customFields,
  onSelectChange,
  onFilterChange,
  selected,
  value,
  innerRef,
}) => {
  const formRef = useRef<Form>(null);
  const validate = useCallback(async () => {
    if (formRef.current) {
      if (await formRef.current.checkValidity()) {
        return true;
      }
      return false;
    }
    return true;
  }, []);
  useImperativeHandle(innerRef, () => ({
    validate,
  }));
  const totalFields = useMemo(() => [...systemFields, ...customFields], [customFields, systemFields]);
  const selectedFields = useMemo(() => selected.reduce((result: IFilterField[], code) => {
    const field = find(totalFields, { code });
    if (field) {
      result.push(field as IFilterField);
    }
    return result;
  }, []), [selected, totalFields]);
  return (
    <>
      <Form ref={formRef}>
        {selectedFields.map((field) => (
          <div key={field.code} style={{ display: 'flex', alignItems: 'center' }}>
            <Field
              mode="filter"
              field={field}
              label={field.title}
              required={field.required}
              value={value[field.code]}
              onChange={(v: any) => {
                onFilterChange(field.code, v);
              }}
            />
            <Button
              icon="delete"
              style={{ marginLeft: 10, flexShrink: 0 }}
              onClick={() => {
                onSelectChange([field.code], false);
              }}
            />
          </div>
        ))}
      </Form>
      <SelectField
        systemFields={systemFields}
        customFields={customFields}
        value={selected}
        onChange={onSelectChange}
      />
    </>
  );
};
export { useFilter };
export default Filter;
