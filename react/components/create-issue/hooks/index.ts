import { IssueValueHook } from '../interface';
import dealWithFeatureVO from './dealWithFeatureVO';
import dealWithWSJFVO from './dealWithWSJFVO';
import dealWithVersionField from './dealWithVersionField';
import dealWithDefaultZeroFields from './dealWithDefaultZeroFields';
import dealWithSubBug from './dealWithSubBug';
import dealWithLabel from './dealWithLabel';
import dealWithFormatPredefinedDateFields from './dealWithFormatPredefinedDateFields';
import dealWithWaterfallIssueVO from './dealWithWaterfallIssueVO';
import dealWithRiskVO from './dealWithRiskVO';

const hooks: IssueValueHook[] = [
  dealWithFeatureVO,
  dealWithWSJFVO,
  dealWithVersionField,
  dealWithDefaultZeroFields,
  dealWithSubBug,
  dealWithLabel,
  dealWithFormatPredefinedDateFields,
  dealWithWaterfallIssueVO,
  dealWithRiskVO,
];
export default hooks;
