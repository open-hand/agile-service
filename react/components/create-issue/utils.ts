import { FieldProps } from 'choerodon-ui/pro/lib/data-set/interface';
import { findIndex } from 'lodash';

export function insertField(fields: FieldProps[], insertFields: {
  /** 是否插入 */
  insert: boolean
  /** 在哪个字段后面 */
  after: string
  field: FieldProps
}[]) {
  insertFields.forEach((f) => {
    const { insert, after, field } = f;
    const index = findIndex(fields, ((fieldOption) => fieldOption.name === after));
    if (insert && index > -1) {
      fields.splice(index + 1, 0, field);
    }
  });
  return fields;
}
