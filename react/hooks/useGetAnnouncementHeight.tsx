import { stores } from '@choerodon/boot';

const { HeaderStore } = stores;

const useGetAnnouncementHeight = () => {
  const existAnnouncement = HeaderStore.existAnnouncement();
  return existAnnouncement ? 'var(--banner-height)' : '0px';
};

export default useGetAnnouncementHeight;
