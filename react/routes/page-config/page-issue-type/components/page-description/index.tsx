import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import { text2Delta } from '@/utils/richText';
import { usePageIssueTypeStore } from '../../stores';

function PageDescription() {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const handleChangeDes = (val: Array<any>) => {
    pageIssueTypeStore.changeTemplate(val);
  };
  const defaultValue = useMemo(() => text2Delta(pageIssueTypeStore.descriptionObj.originTemplate), [pageIssueTypeStore.descriptionObj.originTemplate]);
  return (
    <WYSIWYGEditor
      style={{ height: '100%', width: '100%' }}
      onChange={handleChangeDes}
      value={text2Delta(pageIssueTypeStore.descriptionObj.template?.slice())}
      defaultValue={defaultValue}
      placeholder="您可以在此自定义描述信息格式"
    />
  );
}
export default observer(PageDescription);
