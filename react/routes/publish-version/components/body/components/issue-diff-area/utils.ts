import { publishVersionApi } from '@/api';

async function sequenceRequest(requests: Array<() => Promise<any[]>>, i: number): Promise<any[]> {
  let data = [];
  if (requests[i]) { /** 0[5]  --> 1[4] */
    data = await requests[i]().then(async (res) => {
      console.log('nextData', res);

      const nextData = await sequenceRequest(requests, i + 1);
      return (res || []).concat(nextData);
    });
  }
  return data;
}
export async function requestPreviewData(publishVersionId: string, tagData: any[]): Promise<any[]> {
  const requestStack: Array<() => Promise<any[]>> = tagData.map((data) => {
    console.log('requestPreviewData oneData..requestStack', data);
    return () => publishVersionApi.comparePreviewTag(publishVersionId, data);
  });
  const tableData = await sequenceRequest(requestStack, 0);
  return tableData;
}
