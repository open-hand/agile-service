import { castArray, map } from 'lodash';
import { IIssueType } from '@/common/types';
import { getApplyType } from '@/utils/common';

interface IUseIsProgramIssueTypeProps {
  typeCode?: string | string[]
  issueTypes?: IIssueType | IIssueType[]
  applyType?: 'agile' | 'program'
  /** all 全匹配 part 部分匹配
   * @default 'all'
   * */
  matchMode?: 'all' | 'part'
}
/**
 *  判断传入的问题类型是否全为/部分为项目群问题类型
 * @param config
 * @returns
 */
function isProgramIssueType(config: IUseIsProgramIssueTypeProps): boolean {
  const issueTypes = castArray(config.issueTypes).filter(Boolean) as IIssueType[];
  const typeCodes = [...castArray(config.typeCode), ...map(issueTypes, 'typeCode')].filter(Boolean);
  if (typeCodes.length) {
    const programIssueTypeCodes = (config.applyType ?? getApplyType(true)) === 'agile' ? ['feature'] : ['issue_epic', 'feature'];
    return config.matchMode === 'part' ? typeCodes.some((item) => programIssueTypeCodes.includes(item)) : !typeCodes.some((item) => !programIssueTypeCodes.includes(item));
  }
  return false;
}
function useIsProgramIssueType(config: IUseIsProgramIssueTypeProps) {
  return { isProgramIssueType: isProgramIssueType(config) };
}

// export {}
export default useIsProgramIssueType;
