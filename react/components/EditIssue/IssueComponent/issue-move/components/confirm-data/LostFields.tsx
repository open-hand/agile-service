import { IField, Issue } from '@/common/types';
import React from 'react';
import { ILoseItems } from './Confirm';

interface LostFieldsProps {
  lostFields: IField[]
  loseItems: ILoseItems
  issue: Issue
  targetProject: {
    name: string
  }
}
const loseItemMap = new Map([
  ['test', '测试用例'],
  ['doc', '文档'],
  ['backlog', '需求'],
  ['linkIssue', '工作项'],
]);
const LostFields: React.FC<LostFieldsProps> = ({
  loseItems, lostFields, targetProject, issue,
}) => {
  const loseItemNames = [...Object.entries(loseItems)].filter(([k, v]) => v).map(([k, v]) => loseItemMap.get(k));
  const lostFieldsText = lostFields.length > 0 ? `，${lostFields.map((item) => `【${item.name}】`).join('、')}的字段值将永久丢失。` : '';
  const lostLinksText = loseItemNames.length > 0 ? `${lostFields.length > 0 ? '且' : '，'}该工作项与其他${loseItemNames.join('、')}的关联关系，将永久丢失。` : '';
  if (loseItemNames.length === 0 && lostFields.length === 0) {
    return null;
  }
  return (
    <div>
      {`${issue.issueNum}将移动到【${targetProject?.name}】中 ${lostFieldsText} ${lostLinksText}` }
    </div>
  );
};

export default LostFields;
