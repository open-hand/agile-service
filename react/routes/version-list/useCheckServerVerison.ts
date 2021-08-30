import { useEffect, useState } from 'react';
import { get } from '@choerodon/inject';
import { getOrganizationId } from '@/utils/common';
/**
 *  当前版本为 普通版 还是 其他版本 HOOK
 * @returns
 */
function useCheckCurrentService(): { loading: boolean, version?: 'sass-normal' | 'sass-senior' | 'business' } {
  const [version, setVersion] = useState<{ loading: boolean, version?: 'sass-normal' | 'sass-senior' | 'business' }>({ loading: true, version: undefined });
  useEffect(() => {
    /**
     * 检查是否为普通版
     * @returns  返回 `true` 代表是普通版， 其他版本则都不返回数据
     */
    async function checkUpgrade() {
      if (get('base-sass:heckUpgrade')) {
        const res = await get('base-sass:checkUpgrade')(getOrganizationId());
        return res;
      }
      return false;
    }
    checkUpgrade().then((res) => {
      setVersion({ loading: false, version: res ? 'sass-normal' : 'business' });
    });
  }, []);

  return version;
}
export default useCheckCurrentService;
