module Scopes = Gcloud.Batch.Scopes

module V1 = struct
  include Gcloud.Batch.V1
  include Gcloud.Batch.V1.Make (Async_task_direct) (Client_ezcurl.Default)
end

module V1alpha = struct
  include Gcloud.Batch.V1alpha
  include Gcloud.Batch.V1alpha.Make (Async_task_direct) (Client_ezcurl.Default)
end
