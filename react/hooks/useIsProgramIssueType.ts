import { castArray, map } from 'lodash';
import { IIssueType } from '@/common/types';
import { getApplyType } from '@/utils/common';

interface IUseIsProgramIssueTypeProps {
  typeCode?: string | string[]
  issueTypes?: IIssueType | IIssueType[]
  applyType?: 'agile' | 'program'
}
/**
 *  根据问题类型判断是否为子项目问题类型
 * @param config
 * @returns
 */
function isProgramIssueType(config: IUseIsProgramIssueTypeProps): boolean {
  const issueTypes = castArray(config.issueTypes).filter(Boolean) as IIssueType[];
  const typeCodes = [...castArray(config.typeCode), ...map(issueTypes, 'typeCode')].filter(Boolean);
  if (typeCodes.length) {
    const programIssueTypeCodes = (config.applyType ?? getApplyType(true)) === 'agile' ? ['feature'] : ['issue_epic', 'feature'];
    return issueTypes.some((item) => programIssueTypeCodes.includes(item.typeCode));
  }
  return false;
}
function useIsProgramIssueType(config: IUseIsProgramIssueTypeProps) {
  return { isProgramIssueType: isProgramIssueType(config) };
}

// export {}
export default useIsProgramIssueType;
