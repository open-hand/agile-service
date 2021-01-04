import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Button } from 'choerodon-ui/pro';
import CustomField, { IField } from '@/components/custom-field';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { useDetailContext } from '../../../context';
import Field from '../field';
import StartTime from '../start-time';
import Status from '../status';
import Priority from '../priority';
import Assignee from '../assignee';
import CreateDate from '../create-date';
import UpdateDate from '../update-date';
import EndTime from '../end-time';
import styles from './index.less';

const hideFields = ['priority', 'component', 'label', 'fixVersion', 'sprint', 'timeTrace', 'assignee'];

const CustomFields: React.FC = () => {
  const {
    id, store, outside, disabledDetailEdit, applyType,
  } = useDetailContext();
  const { typeCode } = store.issue;
  const [expand, setExpand] = useState(false);
  useEffect(() => {
    if (typeCode) {
      store.getCustomFields(id, typeCode);
    }
  }, [id, store, typeCode]);
  const handleSubmit = (value: any, field: IField) => {
    store.updateCustomField(field, value);
  };
  let fields = applyType === 'program' ? store.customFields.filter((item) => hideFields.indexOf(item.fieldCode) === -1) : store.customFields;
  // 系统字段单独控制是否显示
  if (typeCode === 'sub_task') {
    fields = fields.filter((field) => ['epic'].indexOf(field.fieldCode) === -1);
  } else if (typeCode === 'issue_epic') {
    fields = fields.filter((field) => field.fieldCode !== 'epic');
  }
  if (!expand) {
    fields = fields.slice(0, 4);
  }
  const renderFields = () => (
    <div>
      <div className={styles.fields}>
        {fields
          .map((field) => {
            switch (field.fieldCode) {
              case 'status':
                return <Status readonly={disabledDetailEdit} />;
              case 'assignee':
                return <Assignee />;
              case 'priority':
                return <Priority readonly={disabledDetailEdit} />;
              case 'creationDate':
                return <CreateDate />;
              case 'lastUpdateDate':
                return <UpdateDate />;
              case 'estimatedStartTime':
                return <StartTime disabled={disabledDetailEdit} />;
              case 'estimatedEndTime':
                return <EndTime disabled={disabledDetailEdit} />;
              default:
                return (
                  <Field label={field.fieldName} key={field.fieldId}>
                    <CustomField
                      className={styles.select}
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
            }
          })}
      </div>
      <Button color={'blue' as ButtonColor} onClick={() => setExpand(!expand)}>
        <span>{expand ? '收起' : '展开'}</span>
        <Icon type={expand ? 'baseline-arrow_drop_up' : 'baseline-arrow_right'} style={{ marginRight: 2 }} />
      </Button>
    </div>
  );
  return renderFields();
};

export default observer(CustomFields);
