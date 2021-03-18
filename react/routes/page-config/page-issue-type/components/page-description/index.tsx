import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import WYSIWYGEditor from '@/components/CKEditor';
import { usePageIssueTypeStore } from '../../stores';

function PageDescription() {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const handleChangeDes = (val: string) => {
    pageIssueTypeStore.changeTemplate(val);
  };
  const defaultValue = useMemo(() => pageIssueTypeStore.descriptionObj.originTemplate, [pageIssueTypeStore.descriptionObj.originTemplate]);
  return (
    <WYSIWYGEditor
      style={{ height: '100%', width: '100%' }}
      onChange={handleChangeDes}
      value={pageIssueTypeStore.descriptionObj.template}
      defaultValue={defaultValue}
      placeholder="您可以在此自定义描述信息格式"
    />
  );
}
export default observer(PageDescription);
