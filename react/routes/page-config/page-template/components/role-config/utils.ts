import { groupBy } from 'lodash';
import { User } from '@/common/types';
/**
 *  主值转换为userId 和roleIds
 * @param value
 * @returns
 */
export function mainValueTransformUserAndRole(value: any, submitProcess: boolean = false) {
  const { newUsers = [], newRoles = [] } = groupBy(value || [], (item) => (String(item).indexOf('role-') === 0 ? 'newRoles' : 'newUsers'));
  const processNewRoles = (newRoles as any[]).map((item: string) => String(item).split('role-')[1]);
  if (newUsers.length > 0 && !submitProcess) {
    processNewRoles.push('specificUser');
  }
  return { users: newUsers as User[], roles: processNewRoles };
}
