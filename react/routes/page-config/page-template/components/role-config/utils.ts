import { groupBy } from 'lodash';
/**
 *  主值转换为userId 和roleIds
 * @param value
 * @returns
 */
export function mainValueTransformUserAndRole(value: any) {
  const { newUserIds = [], newRoles = [] } = groupBy(value || [], (item) => (String(item).indexOf('role-') === 0 ? 'newRoles' : 'newUserIds'));
  const processNewRoles = newRoles.map((item) => String(item).split('role-')[1]);
  if (newUserIds.length > 0) {
    processNewRoles.push('specificUser');
  }
  return { userIds: newUserIds, roles: processNewRoles };
}
