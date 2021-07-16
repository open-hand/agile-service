import { IssueValueHook } from '../interface';
import dealWithFeatureVO from './dealWithFeatureVO';
import dealWithWSJFVO from './dealWithWSJFVO';
import dealWithVersionField from './dealWithVersionField';
import dealWithDefaultZeroFields from './dealWithDefaultZeroFields';

const hooks: IssueValueHook[] = [
  dealWithFeatureVO,
  dealWithWSJFVO,
  dealWithVersionField,
  dealWithDefaultZeroFields,
];
export default hooks;
