import React, { useContext, useMemo, forwardRef } from 'react';
import FormContext from 'choerodon-ui/pro/lib/form/FormContext';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
/**
 * 1.5.3UI 升级后 在form下同时存在label和placeholder 会导致异常
 * @param param0
 * @returns
 */
const ComponentCompatibleWrapper: React.FC<any> = forwardRef(({ children }, ref:any) => {
  const formContext = useContext(FormContext);
  const isFilterPlaceholder = useMemo(() => formContext.formNode && (children?.props?.labelLayout === LabelLayout.float || formContext.labelLayout === LabelLayout.float), [children?.props?.labelLayout, formContext.formNode, formContext.labelLayout]);
  if (isFilterPlaceholder) {
    return React.cloneElement(children, { placeholder: undefined, ref });
  }
  return React.cloneElement(children, { ref });
});
export default ComponentCompatibleWrapper;
