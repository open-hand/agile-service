import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import CustomField, { IField } from '@/components/custom-field';
import { useDetailContext } from '../../../context';
import Field from '../field';
import StartTime from '../start-time';
import EndTime from '../end-time';
import fieldStyles from '../index.less';

const filterFields = ['summary', 'belongToBacklog', 'backlogType', 'backlogClassification', 'description', 'urgent', 'progressFeedback', 'email'];
const CustomFields: React.FC = () => {
  const {
    id, store, outside, disabledDetailEdit,
  } = useDetailContext();
  const { typeCode } = store.issue;
  useEffect(() => {
    if (typeCode) {
      store.getCustomFields(id, typeCode);
    }
  }, [id, store, typeCode]);
  const handleSubmit = (value: any, field: IField) => {
    store.updateCustomField(field, value);
  };
  const renderFields = () => (
    <>
      {store.customFields
        .filter((field) => !filterFields.includes(field.fieldCode))
        .map((field) => {
          if (field.fieldCode === 'estimatedStartTime') {
            return <StartTime disabled={disabledDetailEdit} />;
          } if (field.fieldCode === 'estimatedEndTime') {
            return <EndTime disabled={disabledDetailEdit} />;
          }
          return (
            <Field label={field.fieldName} key={field.fieldId}>
              <CustomField
                className={fieldStyles.select}
                field={field}
                name={field.fieldId}
                mode="edit"
                onSubmit={handleSubmit}
                disabled={disabledDetailEdit}
                outside={outside}
                // @ts-ignore
                projectId={outside ? store.projectId : undefined}
              />
            </Field>
          );
        })}
    </>
  );
  return renderFields();
};

export default observer(CustomFields);
