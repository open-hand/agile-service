import { stores } from '@choerodon/boot';
import { useComputed } from 'mobx-react-lite';

const { HeaderStore } = stores;

const useGetAnnouncementHeight = () => {
  const existAnnouncement = HeaderStore.existAnnouncement();
  return existAnnouncement ? 'var(--banner-height)' : '0px';
};

export default useGetAnnouncementHeight;
