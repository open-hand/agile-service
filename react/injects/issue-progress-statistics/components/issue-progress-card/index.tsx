import { useCreation } from 'ahooks';
import React from 'react';
import classNames from 'classnames';
import styles from './index.less';
import bugSvg from '../../images/bug.svg';
import issueSvg from '../../images/issue.svg';
import storySvg from '../../images/story.svg';
import workTimeSvg from '../../images/workTime.svg';

export type IIssueProgressCardType = 'issue' | 'storyPoint' | 'workTime' | 'environmentBug' | 'nonEnvironmentBug'
interface IIssueProgressCardLabel {
  total: string, finish: string, percentage: string
}
type IIssueProgressCardData<T extends IIssueProgressCardLabel = IIssueProgressCardLabel> = { [K in keyof T]: any }
interface IIssueProgressCardProps {
  title?: string
  className?: string
  type: IIssueProgressCardType
  data: IIssueProgressCardData
  format?: (formatData: { type: IIssueProgressCardType, label: IIssueProgressCardLabel, data: IIssueProgressCardData }) => React.ReactNode
}
const PredefinedTypeImage: Record<IIssueProgressCardType, any> = {
  issue: issueSvg,
  environmentBug: bugSvg,
  nonEnvironmentBug: bugSvg,
  storyPoint: storySvg,
  workTime: workTimeSvg,
};
const PredefinedTypeTitle: Record<IIssueProgressCardType, string> = {
  issue: '工作项',
  environmentBug: '生产环境bug',
  nonEnvironmentBug: '非生产环境bug',
  storyPoint: '故事点',
  workTime: '工时',
};
const PredefinedTypeTemplate: Record<IIssueProgressCardType, IIssueProgressCardLabel> = {
  issue: { total: '总数', finish: '已完成', percentage: '完成率' },
  storyPoint: { total: '总数', finish: '已完成', percentage: '完成率' },
  workTime: { total: '已登记', finish: '预估', percentage: '偏差率' },
  environmentBug: { total: '已修复', finish: '总数', percentage: '修复率' },
  nonEnvironmentBug: { total: '已修复', finish: '总数', percentage: '修复率' },
};
function defaultFormat({ label, data }: { type: IIssueProgressCardType, label: IIssueProgressCardLabel, data: IIssueProgressCardData }) {
  return Object.keys(label).map((key: keyof IIssueProgressCardLabel) => (

    <tr key={key} className={styles.line}>
      <td className={styles.line_label}>{label[key]}</td>
      <td className={styles.line_text}>{data[key]}</td>
    </tr>
  ));
}
const IssueProgressCard: React.FC<IIssueProgressCardProps> = ({
  type, data, title, format: propsFormat, className,
}) => {
  const format = useCreation(() => propsFormat || defaultFormat, []);
  const template = useCreation(() => PredefinedTypeTemplate[type], [type]);
  return (
    <div className={classNames(styles.card, className)}>
      <div className={styles.card_blank} />
      <div className={styles.card_content}>
        <div className={styles.left}>
          <div className={styles.img} style={{ backgroundImage: `url(${PredefinedTypeImage[type]})` }} />
          <span>{title || PredefinedTypeTitle[type]}</span>
        </div>
        <table className={styles.right}>
          {format({ type, label: template, data })}
        </table>
      </div>
      <div className={styles.card_blank} />

    </div>
  );
};
IssueProgressCard.defaultProps = {
  title: undefined,
  format: undefined,
};
const IssueProgressCardPredefined = {
  PredefinedTypeTemplate,
  PredefinedTypeTitle,
} as const;
export { IssueProgressCardPredefined };
export default IssueProgressCard;
