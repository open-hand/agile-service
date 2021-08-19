import getFilterFields from './layouts/filter';
import getSearchFields from './layouts/search';
import getFieldsInstance, { getAgileFields } from './base';
/**
 * 各类字段布局
 */
const FieldProLayout = {
  getFilterFields,
  getSearchFields,
};
export { FieldProLayout, getFieldsInstance, getAgileFields };
