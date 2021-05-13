import { publishVersionApi } from '@/api';

export async function requestPreviewData(publishVersionId: string, tagData: any[]) {
  const tableData: any[] = [];

  tagData.forEach(async (data) => {
    const oneData = await publishVersionApi.comparePreviewTag(publishVersionId, tagData);
    tableData.push(...oneData);
  });
  return tableData;
}
