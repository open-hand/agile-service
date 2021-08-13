import React, { useState } from 'react';
import getDemoFields from '../base';
/**
 *  获取过滤的字段
 * @param fields
 * @param fieldCodeProps
 */
function getFilterFields(fields: Parameters<typeof getDemoFields>[0]) {
  return getDemoFields(fields).map((i) => i[0]);
  //   const newFilters = fields.map((field) => {

  //   }) as any[];
  //   return getFields<FilterField, keyof FilterField>(newFilters).map((i, index) => React.cloneElement(i[1] as JSX.Element,
  //     { ...getProps(fields[(i[0] as any).code], fieldCodeProps) })) as JSX.Element[];
}
const Page: React.FC = () => {
  const [fields, setFields] = useState([]);
  return (
    <div>
      {getFilterFields([{
        code: 'sprint', props: { hasUnassign: false, clearButton: false }, outputs: ['element'], fieldType: 'multiple',
      }, {
        code: 'sprint', props: { hasUnassign: true, multiple: true }, outputs: ['element'], fieldType: 'multiple',
      }, {
        code: 'label', props: {}, fieldType: 'multiple', outputs: ['element'],
      }, {
        fieldType: 'member', code: 'stm', props: {}, isCustom: true, outputs: ['element'],
      },
      ])}
    </div>
  );
};
export default Page;
